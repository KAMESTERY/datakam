package model

import (
	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/dynamodb"
	"github.com/aws/aws-sdk-go/service/dynamodb/dynamodbattribute"

	"slapman/utils"
)

type AwsItem struct {
	Key  int
	Desc string
	Data map[string]interface{}
}

type AwsConfig struct {
	Table  string // required
	Region string // optional
	Limit  int64  // optional
}

func NewAwsSession(cfg *AwsConfig) (sess *session.Session) {
	// Create the config specifying the Region for the DynamoDB table.
	// If config.Region is not set the region must come from the shared
	// config or AWS_REGION
	awsCfg := &aws.Config{}
	if len(cfg.Region) > 0 {
		awsCfg.WithRegion(cfg.Region)
	}

	// Create the session that an Aws Service will use
	sess = session.Must(session.NewSession(awsCfg))
	return
}

func ScanItems(cfg *AwsConfig) (items []AwsItem, err error) {

	// Create the session that the DynamoDB service will use
	sess := NewAwsSession(cfg)

	// Create the DynamoDB service client to make the query request with.
	svc := dynamodb.New(sess)

	// Build the query input parameters
	params := &dynamodb.ScanInput{
		TableName: aws.String(cfg.Table),
	}
	if cfg.Limit > 0 {
		params.Limit = aws.Int64(cfg.Limit)
	}

	// Make the DynamoDB Query API call
	result, err := svc.Scan(params)
	if err != nil {
		utils.Errorf(nil, "ERROR:::: Failed to make Query API call, %+v", err)
	}

	// Unmarshal the Items field in the result value to the Item Go type.
	err = dynamodbattribute.UnmarshalListOfMaps(result.Items, &items)
	if err != nil {
		utils.Errorf(nil, "ERROR:::: Failed to unmarshal Query result items, %+v", err)
	}
	return
}
