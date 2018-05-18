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

	"os"

	"github.com/graphql-go/graphql"
)

const (
	defaultRegion  = endpoints.UsEast1RegionID
	defaultLimit   = 24
	defaultPageNum = 0
	limitKey       = 17
	pageKey        = 18
)

var (
	aws_logger = NewLogger("utilsaws")

	DynaParamInputType = graphql.NewInputObject(graphql.InputObjectConfig{
		Name: "DynaParam",
		Fields: graphql.InputObjectConfigFieldMap{
			"field": &graphql.InputObjectFieldConfig{
				Type:        graphql.NewNonNull(graphql.String),
				Description: "The DynamoDB Parameter Field to Query",
			},
			"value": &graphql.InputObjectFieldConfig{
				Type:        graphql.NewNonNull(graphql.String),
				Description: "The DynamoDB Parameter Value to Use",
			},
		},
	})

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

	DynaQueryAuthArgs = graphql.FieldConfigArgument{
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
		"token": &graphql.ArgumentConfig{
			Type:        graphql.NewNonNull(graphql.String),
			Description: "The JWT Token",
		},
	}

	sess *session.Session
)

func init() {
	sess = newAwsSession()
}

//NewAwsSession creates a new Session for an AWS Service
func newAwsSession() (sess *session.Session) {

	// Grab Region from Environment
	region := os.Getenv("REGION")
	if len(region) < 1 {
		aws_logger.Warnf("WARNING:::: Using Default AWS Region: +%v", defaultRegion)
		region = defaultRegion
	}

	// Create the config specifying the Region for the DynamoDB table.
	// If config.Region is not set the region must come from the shared
	// config or AWS_REGION
	awsCfg := &aws.Config{}
	if len(region) > 0 {
		awsCfg.WithRegion(region)
	}
	aws_logger.Debugf("AWS Config: %+v", awsCfg)

	// // Create the session that an Aws Service will use
	sess = session.Must(session.NewSession(awsCfg))
	// sess = session.Must(session.NewSessionWithOptions(session.Options{
	// 	SharedConfigState: session.SharedConfigEnable,
	// }))
	aws_logger.Debugf("AWS Session: %+v", sess)

	return
}

///////////////////////////////////// SCANNING AWS DYNAMODB

// DynaResolveScanItems scans DynamoDB Items
func DynaResolveScanItems(p graphql.ResolveParams, tableName string) (int, interface{}, error) {

	// Set the current context
	ctx := p.Context
	ctx = context.WithValue(ctx, limitKey, p.Args["limit"])

	rows, err := dynaScanItems(ctx, tableName)
	if err != nil {
		return 0, nil, err
	}

	count := len(rows)

	aws_logger.Debugf("%+v Rows: %+v", count, rows)

	return count, rows, nil
}

func dynaScanItems(ctx context.Context, tableName string) ([]map[string]interface{}, error) {

	// Create the DynamoDB service client to make the query request with.
	svc := dynamodb.New(sess)

	// Build the query input parameters
	params := &dynamodb.ScanInput{
		TableName: aws.String(tableName),
	}

	limit, ok := ctx.Value(limitKey).(int)
	if !ok || limit < 0 {
		limit = defaultLimit
		aws_logger.Warnf("WARNING:::: Using Default Limit of: %+v", defaultLimit)
	}
	params.Limit = aws.Int64(int64(limit))

	aws_logger.Debugf("Params: %+v", params)

	// Make the DynamoDB Query API call
	result, err := svc.ScanWithContext(ctx, params)
	if err != nil {
		scanError := errors.New("Failed to make DynamoDB Query API call")
		aws_logger.Errorf("ERROR:::: %+v", err)
		return nil, scanError
	}

	aws_logger.Debugf("Result: %+v", result)

	var rows []map[string]interface{}
	// Unmarshal the Items field in the result value to the Item Go type.
	err = dynamodbattribute.UnmarshalListOfMaps(result.Items, &rows)
	if err != nil {
		unmarshalError := errors.New("Failed to unmarshal Query result items")
		aws_logger.Errorf("ERROR:::: %+v", unmarshalError)
		return nil, unmarshalError
	}

	aws_logger.Debugf("Rows: %+v", rows)

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

// DynaResolveScanPages scans DynamoDB Items
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

	aws_logger.Debugf("%+v Rows: %+v", count, rows)

	return count, rows, nil
}

func dynaScanPages(ctx context.Context, tableName string) (rows []map[string]interface{}, err error) {

	aws_logger.Debugf("PAGE SCAN CONTEXT: %+v", ctx)

	// Create the DynamoDB service client to make the query request with.
	svc := dynamodb.New(sess)

	// Build the query input parameters
	params := &dynamodb.ScanInput{
		TableName: aws.String(tableName),
	}

	limit, ok := ctx.Value(limitKey).(int)
	if !ok || limit < 0 {
		limit = defaultLimit
		aws_logger.Warnf("WARNING:::: Using Default Limit of: %+v", defaultLimit)
	}
	params.Limit = aws.Int64(int64(limit))

	pageNum, ok := ctx.Value(pageKey).(int)
	if !ok || pageNum < 0 {
		pageNum = defaultPageNum
		aws_logger.Warnf("WARNING:::: Retrieving Default Page #%+v", defaultPageNum)
	}

	aws_logger.Debugf("Params: %+v", params)

	currentPage := 0
	// Make the DynamoDB Query API call
	err = svc.ScanPagesWithContext(
		ctx,
		params,
		func(page *dynamodb.ScanOutput, lastPage bool) bool {

			if currentPage == pageNum {
				aws_logger.Debugf("Page #%+v Result: %+v", pageNum, page)

				// Unmarshal the Items field in the result value to the Item Go type.
				err = dynamodbattribute.UnmarshalListOfMaps(page.Items, &rows)
				if err != nil {
					err = errors.New("Failed to unmarshal Query result items")
					aws_logger.Errorf("ERROR:::: %+v", err)
					return false // stop paging
				}

				aws_logger.Debugf("Page #%+v Rows: %+v", pageNum, rows)

				return false // stop paging
			}

			currentPage++

			return true // keep paging
		},
	)
	if err != nil {
		scanError := errors.New("Failed to make DynamoDB Query API call")
		aws_logger.Errorf("SCAN PAGES ERROR:::: %+v", err)
		return nil, scanError
	}

	return
}

///////////////////////////////////// PUTTING AWS DYNAMODB

// DynaResolvePutItem puts DynamoDB Items
func DynaResolvePutItem(p graphql.ResolveParams, tableName string, data interface{}) (interface{}, error) {

	// Set the current context
	ctx := p.Context

	return dynaPutItem(ctx, tableName, data)
}

func dynaPutItem(ctx context.Context, tableName string, data interface{}) (success interface{}, err error) {

	aws_logger.Debugf("Putting Data: %+v", data)

	// Create the DynamoDB service client to make the query request with.
	svc := dynamodb.New(sess)

	dataItem, err := dynamodbattribute.MarshalMap(data)
	if err != nil {
		aws_logger.Errorf("ERROR:::: %+v \nCould not put %+v", err, data)
		return
	}

	// Build the query input parameters
	params := &dynamodb.PutItemInput{
		Item:      dataItem,
		TableName: aws.String(tableName),
	}
	aws_logger.Debugf("Params: %+v", params)

	// Now put the data item, either logging or discarding the result
	success, err = svc.PutItemWithContext(ctx, params)
	if err != nil {
		if err.(awserr.Error).Code() == dynamodb.ErrCodeProvisionedThroughputExceededException {
			aws_logger.Warn("WARNING:::: The provisioned Throughput has been Exceeded")
		}
		aws_logger.Errorf("Error inserting %v (%v)", data, err)
		return
	}
	aws_logger.Debugf("PUT ITEM SUCCESS:::: %+v", success)

	return
}

///////////////////////////////////// UPDATING AWS DYNAMODB

// DynaResolveUpdateItem updates DynamoDB Items
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
			aws_logger.Errorf("ERROR:::: UpdateItem Marshal ERROR: %+v", marshalErr)
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
			aws_logger.Errorf("ERROR:::: UpdateItem Marshal ERROR: %+v", marshalErr)
			return nil, marshalErr
		}
		attributeValues[kVal] = attrVal
	}
	updateExpression = aws.String("SET " + strings.Join(upExpChunks, ", "))

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

	aws_logger.Debugf("Params: %+v", params)

	// Now put the data item, either logging or discarding the result
	result, err := svc.UpdateItemWithContext(ctx, params)
	if err != nil {
		if err.(awserr.Error).Code() == dynamodb.ErrCodeProvisionedThroughputExceededException {
			aws_logger.Warn("WARNING:::: The provisioned Throughput has been Exceeded")
		}
		aws_logger.Errorf("Error inserting %v (%v)", params, err)
		return
	}
	aws_logger.Debugf("UPDATE ITEM SUCCESS:::: %+v", result)

	updatedItem := make(map[string]interface{})
	// Unmarshal the Updated Item field in the result value to the Item Go type.
	for upKey, upAttr := range result.Attributes {
		var upVal interface{}
		err = dynamodbattribute.Unmarshal(upAttr, &upVal)
		if err != nil {
			unmarshalError := errors.New("Failed to unmarshal Update result items")
			aws_logger.Errorf("ERROR:::: %+v", unmarshalError)
			return nil, unmarshalError
		}
		aws_logger.Debugf("UPVAL: %+v", upVal)
		updatedItem[upKey] = upVal
	}

	if len(updatedItem) > 0 {
		success = updatedItem
	}
	aws_logger.Debugf("Updated Item: %+v", success)

	return
}

///////////////////////////////////// GETTING ITEM AWS DYNAMODB

func DynaResolveGetItem(p graphql.ResolveParams, tableName string, keyData map[string]interface{}) (interface{}, error) {

	// Set the current context
	ctx := p.Context
	ctx = context.WithValue(ctx, limitKey, p.Args["limit"])

	return dynaGetItem(ctx, tableName, keyData)
}

func dynaGetItem(ctx context.Context, tableName string, keyData map[string]interface{}) (success interface{}, err error) {

	keyMap := make(map[string]*dynamodb.AttributeValue)

	for key, val := range keyData {
		keyAttr, marshalErr := dynamodbattribute.Marshal(val)
		if marshalErr != nil {
			aws_logger.Errorf("ERROR:::: GetItem Marshal ERROR: %+v", marshalErr)
			return nil, marshalErr
		}
		keyMap[key] = keyAttr
	}

	// Create the DynamoDB service client to make the query request with.
	svc := dynamodb.New(sess)

	params := &dynamodb.GetItemInput{
		TableName: aws.String(tableName),
		Key:       keyMap,
	}

	aws_logger.Debugf("Params: %+v", params)

	// Now get the data item, either logging or discarding the result
	result, err := svc.GetItemWithContext(ctx, params)
	if err != nil {
		if err.(awserr.Error).Code() == dynamodb.ErrCodeProvisionedThroughputExceededException {
			aws_logger.Warn("WARNING:::: The provisioned Throughput has been Exceeded")
		}
		aws_logger.Errorf("Error retrieving %v (%v)", params, err)
		return
	}
	aws_logger.Debugf("GET ITEM SUCCESS:::: %+v", result)

	getItem := make(map[string]interface{})
	// Unmarshal the Item from the result value into the Item Go type.
	for itemKey, itemAttr := range result.Item {
		var itemVal interface{}
		err = dynamodbattribute.Unmarshal(itemAttr, &itemVal)
		if err != nil {
			unmarshalError := errors.New("Failed to unmarshal Update result items")
			aws_logger.Errorf("ERROR:::: %+v", unmarshalError)
			return nil, unmarshalError
		}
		aws_logger.Debugf("ITEMVAL: %+v", itemVal)
		getItem[itemKey] = itemVal
	}

	if len(getItem) > 0 {
		success = getItem
	}
	aws_logger.Debugf("Get Item: %+v", success)

	return
}

///////////////////////////////////// QUERYING AWS DYNAMODB

// DynaResolveQuery queries DynamoDB
func DynaResolveQuery(p graphql.ResolveParams, queryInput *dynamodb.QueryInput) (int, []map[string]interface{}, error) {

	// Set the current context
	ctx := p.Context
	ctx = context.WithValue(ctx, limitKey, p.Args["limit"])

	rows, err := dynaQuery(ctx, queryInput)
	if err != nil {
		return 0, nil, err
	}

	count := len(rows)

	return count, rows, nil
}

func DynaResolveOneQuery(p graphql.ResolveParams, queryInput *dynamodb.QueryInput) (map[string]interface{}, error) {

	// Set the current context
	ctx := p.Context
	ctx = context.WithValue(ctx, limitKey, p.Args["limit"])

	rows, err := dynaQuery(ctx, queryInput)
	if err != nil {
		return nil, err
	}

	if count := len(rows); count >= 1 {
		return rows[0], nil
	} else {
		return nil, nil
	}
}

func dynaQuery(ctx context.Context, queryInput *dynamodb.QueryInput) (success []map[string]interface{}, err error) {

	aws_logger.Debugf("Query Input: %+v", queryInput)

	// Create the DynamoDB service client to make the query request with.
	svc := dynamodb.New(sess)

	limit, ok := ctx.Value(limitKey).(int)
	if !ok || limit < 0 {
		limit = defaultLimit
		aws_logger.Warnf("WARNING:::: Using Default Limit of: %+v", defaultLimit)
	}
	queryInput.Limit = aws.Int64(int64(limit))

	// Now run the Query
	result, err := svc.QueryWithContext(ctx, queryInput)
	if err != nil {
		queryError := errors.New("Failed to make DynamoDB Query API call")
		aws_logger.Errorf("ERROR:::: %+v", err)
		return nil, queryError
	}

	aws_logger.Debugf("Result: %+v", result)

	var rows []map[string]interface{}
	// Unmarshal the Items field in the result value to the Item Go type.
	err = dynamodbattribute.UnmarshalListOfMaps(result.Items, &rows)
	if err != nil {
		unmarshalError := errors.New("Failed to unmarshal Query result items")
		aws_logger.Errorf("ERROR:::: %+v", unmarshalError)
		return nil, unmarshalError
	}

	aws_logger.Debugf("Rows: %+v", rows)

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

	aws_logger.Debugf("QUERY PARAMS: %+v", params)

	return
}

// https://aws.amazon.com/blogs/developer/category/go/

// QueryDsl builds Query Parameters for DynamoDB
type QueryDsl struct {
	dynamodb.QueryInput
	Context   context.Context
	ErrorList []string
}

// DynaQueryDsl instantiate a QueryDsl
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
		aws_logger.Warnf("WARNING:::: Using Default Limit of: +%v", defaultLimit)
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

// Build builds a QueryDsl from the input data
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

// WithLimit sets the limit for the query being built
func (qi *QueryDsl) WithLimit(limit int) *QueryDsl {
	qi.QueryInput.Limit = aws.Int64(int64(limit))
	return qi
}

// WithParam adds parameter to the Query DSL
func (qi *QueryDsl) WithParam(field, operation string, value interface{}) *QueryDsl {
	qi.with(
		field,
		operation,
		value,
	)
	return qi
}

// AsInput converts the QueryDsl into a QueryInput
func (qi *QueryDsl) AsInput() (*dynamodb.QueryInput, error) {

	if len(qi.ErrorList) > 0 {
		errorMessages := strings.Join(qi.ErrorList, ",\n ")
		dslError := fmt.Errorf("ERRORS:::: %+v", errorMessages)
		aws_logger.Errorf("ERROR:::: QUERY DSL ERROR: %+v", dslError)
		return nil, dslError
	}

	return &qi.QueryInput, nil
}
