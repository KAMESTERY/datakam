package utils

import (
	"context"
	"errors"
	"fmt"
	"strings"

	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/awserr"
	"github.com/aws/aws-sdk-go/aws/endpoints"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/dynamodb"
	"github.com/aws/aws-sdk-go/service/dynamodb/dynamodbattribute"

	"github.com/graphql-go/graphql"
)

const (
	defaultRegion = endpoints.UsEast1RegionID
	defaultLimit  = 24
	regionKey     = 16
	limitKey      = 16
	maxGoroutines = 20
)

//NewAwsSession: Creates a new Session for an AWS Service
func NewAwsSession(ctx context.Context) (sess *session.Session) {

	// Grab Region from Context
	region, ok := ctx.Value(regionKey).(string)
	if !ok {
		Warnf(nil, "WARNING:::: Using Default AWS Region: +%v", defaultRegion)
		region = defaultRegion
	}

	// Create the config specifying the Region for the DynamoDB table.
	// If config.Region is not set the region must come from the shared
	// config or AWS_REGION
	awsCfg := &aws.Config{}
	if len(region) > 0 {
		awsCfg.WithRegion(region)
	}
	Debugf(nil, "AWS Config: %+v", awsCfg)

	// // Create the session that an Aws Service will use
	sess = session.Must(session.NewSession(awsCfg))
	// sess = session.Must(session.NewSessionWithOptions(session.Options{
	// 	SharedConfigState: session.SharedConfigEnable,
	// }))
	Debugf(nil, "AWS Session: %+v", sess)

	return
}

///////////////////////////////////// SCANNING AWS DYNAMODB

// ScanItems: Scan DynamoDB Items
func DynaResolveScanItems(p graphql.ResolveParams, tableName string) (interface{}, error) {

	// Set the current context
	ctx := p.Context
	region, ok := p.Args["region"].(string)
	if ok {
		ctx = context.WithValue(ctx, regionKey, region)
	}
	limit, ok := p.Args["limit"].(string)
	if ok {
		ctx = context.WithValue(ctx, limitKey, limit)
	}

	rows, err := DynaScanItems(ctx, tableName)
	if err != nil {
		return nil, err
	}

	Debugf(nil, "Rows: %+v", rows)

	return rows, nil
}

// ScanItems: Scan DynamoDB Items
func DynaScanItems(ctx context.Context, tableName string) ([]map[string]interface{}, error) {

	// Create the session that the DynamoDB service will use
	sess := NewAwsSession(ctx)

	// Create the DynamoDB service client to make the query request with.
	svc := dynamodb.New(sess)

	// Build the query input parameters
	params := &dynamodb.ScanInput{
		TableName: aws.String(tableName),
	}

	limit, ok := ctx.Value(limitKey).(int64)
	if ok && limit > 0 {
		params.Limit = aws.Int64(limit)
	} else {
		Warnf(nil, "WARNING:::: Using Default Limit of: +%v", defaultLimit)
		params.Limit = aws.Int64(defaultLimit)
	}

	Debugf(nil, "Params: %+v", params)

	// Make the DynamoDB Query API call
	result, err := svc.ScanWithContext(ctx, params)
	if err != nil {
		scanError := errors.New("Failed to make DynamoDB Query API call")
		Errorf(nil, "ERROR:::: %+v", err)
		return nil, scanError
	}

	Debugf(nil, "Result: %+v", result)

	var rows []map[string]interface{}
	// Unmarshal the Items field in the result value to the Item Go type.
	err = dynamodbattribute.UnmarshalListOfMaps(result.Items, &rows)
	if err != nil {
		unmarshalError := errors.New("Failed to unmarshal Query result items")
		Errorf(nil, "ERROR:::: %+v", unmarshalError)
		return nil, unmarshalError
	}

	Debugf(nil, "Rows: %+v", rows)

	return rows, nil
}

///////////////////////////////////// PUTTING AWS DYNAMODB

// PutItem: Put DynamoDB Items
func DynaResolvePutItem(p graphql.ResolveParams, tableName string, data interface{}) (interface{}, error) {

	// Set the current context
	ctx := p.Context
	region, ok := p.Args["region"].(string)
	if ok {
		ctx = context.WithValue(ctx, regionKey, region)
	}

	return DynaPutItem(ctx, tableName, data)
}

// PutItem: Put DynamoDB Item
func DynaPutItem(ctx context.Context, tableName string, data interface{}) (success interface{}, err error) {

	Debugf(nil, "Putting Data: %+v", data)

	// Create the session that the DynamoDB service will use
	sess := NewAwsSession(ctx)

	// Create the DynamoDB service client to make the query request with.
	svc := dynamodb.New(sess)

	dataItem, err := dynamodbattribute.MarshalMap(data)
	if err != nil {
		Errorf(nil, "ERROR:::: %+v \nCould not put %+v", err, data)
		return
	}

	// Build the query input parameters
	params := &dynamodb.PutItemInput{
		Item:      dataItem,
		TableName: aws.String(tableName),
	}
	Debugf(nil, "Params: %+v", params)

	// Now put the data item, either logging or discarding the result
	success, err = svc.PutItemWithContext(ctx, params)
	if err != nil {
		if err.(awserr.Error).Code() == dynamodb.ErrCodeProvisionedThroughputExceededException {
			Warn(nil, "WARNING:::: The provisioned Throughput has been Exceeded")
		}
		Errorf(nil, "Error inserting %v (%v)", data, err)
		return
	}
	Debugf(nil, "PUT ITEM SUCCESS:::: %+v", success)

	return
}

///////////////////////////////////// UPDATING AWS DYNAMODB

// UpdateItem: Update DynamoDB Items
func DynaResolveUpdateItem(p graphql.ResolveParams, tableName string, keyData, data map[string]interface{}) (interface{}, error) {

	// Set the current context
	ctx := p.Context
	region, ok := p.Args["region"].(string)
	if ok {
		ctx = context.WithValue(ctx, regionKey, region)
	}

	return DynaUpdateItem(ctx, tableName, keyData, data)
}

// UpdateItem: Update DynamoDB Item
func DynaUpdateItem(ctx context.Context, tableName string, keyData, data map[string]interface{}) (success interface{}, err error) {

	var (
		keyMap           = make(map[string]*dynamodb.AttributeValue)
		attributeNames   = make(map[string]*string)
		attributeValues  = make(map[string]*dynamodb.AttributeValue)
		upExpChunks      []string
		updateExpression *string
	)

	for key, val := range keyData {
		keyAttr, marshalErr := dynamodbattribute.Marshal(val)
		if marshalErr != nil {
			Errorf(nil, "ERROR:::: UpdateItem Marshal ERROR: %+v", marshalErr)
			return nil, marshalErr
		}
		keyMap[key] = keyAttr
	}

	// Populate the Attribute Names as well as the Attribute Values and then Generate the Update Expression
	for k, v := range data {
		kName := "#" + k
		kVal := ":" + k + "_value"

		attributeNames[kName] = aws.String(k)
		upExpChunks = append(upExpChunks, fmt.Sprintf("%+v = %+v", kName, kVal))

		attrVal, marshalErr := dynamodbattribute.Marshal(v)
		if marshalErr != nil {
			Errorf(nil, "ERROR:::: UpdateItem Marshal ERROR: %+v", marshalErr)
			return nil, marshalErr
		}
		attributeValues[kVal] = attrVal
	}
	updateExpression = aws.String("SET " + strings.Join(upExpChunks, ", "))

	// Create the session that the DynamoDB service will use
	sess := NewAwsSession(ctx)

	// Create the DynamoDB service client to make the query request with.
	svc := dynamodb.New(sess)

	params := &dynamodb.UpdateItemInput{
		TableName: aws.String(tableName),

		Key: keyMap,

		ExpressionAttributeNames:  attributeNames,
		ExpressionAttributeValues: attributeValues,
		UpdateExpression:          updateExpression,

		ReturnConsumedCapacity:      aws.String("NONE"),
		ReturnItemCollectionMetrics: aws.String("NONE"),
		ReturnValues:                aws.String("ALL_NEW"),
	}

	Debugf(nil, "Params: %+v", params)

	// Now put the data item, either logging or discarding the result
	result, err := svc.UpdateItemWithContext(ctx, params)
	if err != nil {
		if err.(awserr.Error).Code() == dynamodb.ErrCodeProvisionedThroughputExceededException {
			Warn(nil, "WARNING:::: The provisioned Throughput has been Exceeded")
		}
		Errorf(nil, "Error inserting %v (%v)", params, err)
		return
	}
	Debugf(nil, "UPDATE ITEM SUCCESS:::: %+v", result)

	updatedItem := make(map[string]interface{})
	// Unmarshal the Updated Item field in the result value to the Item Go type.
	for upKey, upAttr := range result.Attributes {
		var upVal interface{}
		err = dynamodbattribute.Unmarshal(upAttr, &upVal)
		if err != nil {
			unmarshalError := errors.New("Failed to unmarshal Update result items")
			Errorf(nil, "ERROR:::: %+v", unmarshalError)
			return nil, unmarshalError
		}
		Debugf(nil, "UPVAL: %+v", upVal)
		updatedItem[upKey] = upVal
	}

	success = updatedItem
	Debugf(nil, "Updated Item: %+v", success)

	return
}
