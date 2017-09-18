package utils

import (
	"context"
	"errors"

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

type DynaScanResult struct {
	Table string                   `json:"table"`
	Rows  []map[string]interface{} `json:"rows"`
}

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

// ScanItems: Scan DynamoDB Items
func DynaResolveScanItems(p graphql.ResolveParams, tableName string) (interface{}, error) {

	// Initialize Content
	content := DynaScanResult{}

	content.Table = tableName

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

	content.Rows = rows

	Debugf(nil, "Content: %+v", content)

	return content, nil
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

// PutItem: Scan DynamoDB Items
func DynaResolvePutItem(p graphql.ResolveParams, tableName string, data interface{}) (interface{}, error) {

	// Set the current context
	ctx := p.Context
	region, ok := p.Args["region"].(string)
	if ok {
		ctx = context.WithValue(ctx, regionKey, region)
	}

	// gameScore := struct{
	// 	UserId int64 `json:"UserId"`
	// 	GameTitle string `json:"GameTitle"`
	// 	TopScore int64 `json:"TopScore"`
	// }{}
	// userId, ok := p.Args["UserId"].(string)
	// if ok {
	// 	ctx = context.WithValue(ctx, limitKey, limit)
	// }

	return DynaPutItem(ctx, tableName, data)
}

// PutItem: Put DynamoDB Item
func DynaPutItem(ctx context.Context, tableName string, data interface{}) (success interface{}, err error) {

	// Create the session that the DynamoDB service will use
	sess := NewAwsSession(ctx)

	// Create the DynamoDB service client to make the query request with.
	svc := dynamodb.New(sess)

	dataItem, err := dynamodbattribute.MarshalMap(data)
	if err != nil {
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
				return
			}
			// TODO: Special case...
			Errorf(nil, "Error inserting %v (%v)", data, err)
		}
	} else {
		Errorf(nil, "ERROR:::: %+v", err)
		return
	}

	return
}
