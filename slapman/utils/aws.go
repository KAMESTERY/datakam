package utils

import (
	"context"
	"errors"
	"fmt"
	"strings"

	"github.com/mitchellh/mapstructure"

	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/awserr"
	"github.com/aws/aws-sdk-go/aws/endpoints"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/dynamodb"
	"github.com/aws/aws-sdk-go/service/dynamodb/dynamodbattribute"

	"github.com/graphql-go/graphql"
)

const (
	defaultRegion  = endpoints.UsEast1RegionID
	defaultLimit   = 24
	defaultPageNum = 0
	regionKey      = 16
	limitKey       = 17
	pageKey        = 18
	maxGoroutines  = 20
)

var (
	DynaQueryParamInputType = graphql.NewInputObject(graphql.InputObjectConfig{
		Name: "DynaQueryParam",
		Fields: graphql.InputObjectConfigFieldMap{
			"field": &graphql.InputObjectFieldConfig{
				Type:        graphql.NewNonNull(graphql.String),
				Description: "The DynamoDB Parameter Field to Query",
			},
			"operation": &graphql.InputObjectFieldConfig{
				Type:        graphql.NewNonNull(graphql.String),
				Description: "The DynamoDB Parameter Operation to Use",
			},
			"value": &graphql.InputObjectFieldConfig{
				Type:        graphql.NewNonNull(graphql.String),
				Description: "The DynamoDB Parameter Value to Use",
			},
		},
	})

	DynaQueryArgs = graphql.FieldConfigArgument{
		"table": &graphql.ArgumentConfig{
			Type:        graphql.NewNonNull(graphql.String),
			Description: "The DynamoDB Table to Query",
		},
		"index": &graphql.ArgumentConfig{
			Type:        graphql.NewNonNull(graphql.String),
			Description: "The DynamoDB Table Index to Use",
		},
		"parameters": &graphql.ArgumentConfig{
			Type: graphql.NewList(DynaQueryParamInputType),
			//Type:        graphql.NewList(graphql.String),
			Description: "The DynamoDB Query Parameters to Use",
		},
		"region": &graphql.ArgumentConfig{
			Type: graphql.String,
		},
		"limit": &graphql.ArgumentConfig{
			Type: graphql.Int,
		},
	}
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
func DynaResolveScanItems(p graphql.ResolveParams, tableName string) (int, interface{}, error) {

	// Set the current context
	ctx := p.Context
	ctx = context.WithValue(ctx, limitKey, p.Args["limit"])

	rows, err := dynaScanItems(ctx, tableName)
	if err != nil {
		return 0, nil, err
	}

	count := len(rows)

	Debugf(nil, "%+v Rows: %+v", count, rows)

	return count, rows, nil
}

func dynaScanItems(ctx context.Context, tableName string) ([]map[string]interface{}, error) {

	// Create the session that the DynamoDB service will use
	sess := NewAwsSession(ctx)

	// Create the DynamoDB service client to make the query request with.
	svc := dynamodb.New(sess)

	// Build the query input parameters
	params := &dynamodb.ScanInput{
		TableName: aws.String(tableName),
	}

	limit, ok := ctx.Value(limitKey).(int)
	if !ok || limit < 0 {
		limit = defaultLimit
		Warnf(nil, "WARNING:::: Using Default Limit of: %+v", defaultLimit)
	}
	params.Limit = aws.Int64(int64(limit))

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

// ////////// PAGES

// https://www.google.com/search?q=golang+dynamodb+pagination&oq=golang+dynamodb+pagination&aqs=chrome.0.69i59.12933j0j7&sourceid=chrome&ie=UTF-8
// https://docs.aws.amazon.com/sdk-for-go/api/service/dynamodb/
// http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Scan.html#Scan.Pagination
// https://github.com/aws/aws-sdk-go/issues/58
// https://github.com/aws/aws-sdk-go/blob/master/service/dynamodb/api.go
// http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Query.html#FilteringResults
// http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Query.html#Query.FilterExpression

// ScanPages: Scan DynamoDB Items
func DynaResolveScanPages(p graphql.ResolveParams, tableName string) (int, interface{}, error) {

	// Set the current context
	ctx := p.Context
	ctx = context.WithValue(ctx, limitKey, p.Args["limit"])
	ctx = context.WithValue(ctx, pageKey, p.Args["page"])

	rows, err := dynaScanPages(ctx, tableName)
	if err != nil {
		return 0, nil, err
	}
	count := len(rows)

	Debugf(nil, "%+v Rows: %+v", count, rows)

	return count, rows, nil
}

func dynaScanPages(ctx context.Context, tableName string) (rows []map[string]interface{}, err error) {

	Debugf(nil, "PAGE SCAN CONTEXT: %+v", ctx)

	// Create the session that the DynamoDB service will use
	sess := NewAwsSession(ctx)

	// Create the DynamoDB service client to make the query request with.
	svc := dynamodb.New(sess)

	// Build the query input parameters
	params := &dynamodb.ScanInput{
		TableName: aws.String(tableName),
	}

	limit, ok := ctx.Value(limitKey).(int)
	if !ok || limit < 0 {
		limit = defaultLimit
		Warnf(nil, "WARNING:::: Using Default Limit of: %+v", defaultLimit)
	}
	params.Limit = aws.Int64(int64(limit))

	pageNum, ok := ctx.Value(pageKey).(int)
	if !ok || pageNum < 0 {
		pageNum = defaultPageNum
		Warnf(nil, "WARNING:::: Retrieving Default Page #%+v", defaultPageNum)
	}

	Debugf(nil, "Params: %+v", params)

	currentPage := 0
	// Make the DynamoDB Query API call
	err = svc.ScanPagesWithContext(
		ctx,
		params,
		func(page *dynamodb.ScanOutput, lastPage bool) bool {

			if currentPage == pageNum {
				Debugf(nil, "Page #%+v Result: %+v", pageNum, page)

				// Unmarshal the Items field in the result value to the Item Go type.
				err = dynamodbattribute.UnmarshalListOfMaps(page.Items, &rows)
				if err != nil {
					err = errors.New("Failed to unmarshal Query result items")
					Errorf(nil, "ERROR:::: %+v", err)
					return false // stop paging
				}

				Debugf(nil, "Page #%+v Rows: %+v", pageNum, rows)

				return false // stop paging
			}

			currentPage++

			return true // keep paging
		},
	)
	if err != nil {
		scanError := errors.New("Failed to make DynamoDB Query API call")
		Errorf(nil, "SCAN PAGES ERROR:::: %+v", err)
		return nil, scanError
	}

	return
}

///////////////////////////////////// PUTTING AWS DYNAMODB

// PutItem: Put DynamoDB Items
func DynaResolvePutItem(p graphql.ResolveParams, tableName string, data interface{}) (interface{}, error) {

	// Set the current context
	ctx := p.Context

	return dynaPutItem(ctx, tableName, data)
}

func dynaPutItem(ctx context.Context, tableName string, data interface{}) (success interface{}, err error) {

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

	return dynaUpdateItem(ctx, tableName, keyData, data)
}

func dynaUpdateItem(ctx context.Context, tableName string, keyData, data map[string]interface{}) (success interface{}, err error) {

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

///////////////////////////////////// QUERYING AWS DYNAMODB

// Query: Query DynamoDB
func DynaResolveQuery(p graphql.ResolveParams, queryInput *dynamodb.QueryInput) (int, interface{}, error) {

	// Set the current context
	ctx := p.Context

	rows, err := dynaQuery(ctx, queryInput)
	if err != nil {
		return 0, nil, err
	}

	count := len(rows)

	return count, rows, nil
}

func dynaQuery(ctx context.Context, queryInput *dynamodb.QueryInput) (success []map[string]interface{}, err error) {

	Debugf(nil, "Query Input: %+v", queryInput)

	// Create the session that the DynamoDB service will use
	sess := NewAwsSession(ctx)

	// Create the DynamoDB service client to make the query request with.
	svc := dynamodb.New(sess)

	// Now run the Query
	result, err := svc.QueryWithContext(ctx, queryInput)
	if err != nil {
		queryError := errors.New("Failed to make DynamoDB Query API call")
		Errorf(nil, "ERROR:::: %+v", err)
		return nil, queryError
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

// Query Input Extensions and Helpers

type dynaQueryParam struct {
	Field     string
	Operation string // IN, NULL, BETWEEN, LT, NOT_CONTAINS, EQ, GT, NOT_NULL, NE, LE, BEGINS_WITH, GE, CONTAINS
	Value     interface{}
}

func asDynaQueryParamList(data []interface{}) (params []dynaQueryParam) {

	for _, m := range data {
		var param dynaQueryParam
		mapstructure.Decode(m, &param)
		params = append(params, param)
	}

	Debugf(nil, "QUERY PARAMS: %+v", params)

	return
}

type QueryDsl struct {
	dynamodb.QueryInput
	Context   context.Context
	ErrorList []string
}

func DynaQueryDsl(ctx context.Context, table, index string) *QueryDsl {

	qi := &QueryDsl{
		QueryInput: dynamodb.QueryInput{
			TableName:     aws.String(table),
			IndexName:     aws.String(index),
			KeyConditions: make(map[string]*dynamodb.Condition),
		},
		Context: ctx,
	}

	limit, ok := ctx.Value(limitKey).(int64)
	if ok && limit > 0 {
		qi.Limit = aws.Int64(limit)
	} else {
		Warnf(nil, "WARNING:::: Using Default Limit of: +%v", defaultLimit)
		qi.Limit = aws.Int64(defaultLimit)
	}

	return qi
}

func (qi *QueryDsl) with(field, operator string, value interface{}) *QueryDsl {

	attrVal, marshalErr := dynamodbattribute.Marshal(value)
	if marshalErr != nil {
		marshalErrMsg := fmt.Sprintf("MARSHAL ERROR: %+v", marshalErr.Error())
		qi.ErrorList = append(qi.ErrorList, marshalErrMsg)
	}

	condition := &dynamodb.Condition{
		ComparisonOperator: aws.String(operator),
	}

	condition.AttributeValueList = append(condition.AttributeValueList, attrVal)

	qi.QueryInput.KeyConditions[field] = condition

	return qi
}

func (qi *QueryDsl) Build(data []interface{}) *QueryDsl {
	params := asDynaQueryParamList(data)
	for _, param := range params {
		qi.with(
			param.Field,
			param.Operation,
			param.Value,
		)
	}
	return qi
}

func (qi *QueryDsl) WithLimit(limit int) *QueryDsl {
	qi.QueryInput.Limit = aws.Int64(int64(limit))
	return qi
}

func (qi *QueryDsl) AsInput() (*dynamodb.QueryInput, error) {

	if len(qi.ErrorList) > 0 {
		errorMessages := strings.Join(qi.ErrorList, ",\n ")
		dslError := errors.New(fmt.Sprintf("ERRORS:::: %+v", errorMessages))
		Errorf(nil, "ERROR:::: QUERY DSL ERROR: %+v", dslError)
		return nil, dslError
	}

	return &qi.QueryInput, nil
}
