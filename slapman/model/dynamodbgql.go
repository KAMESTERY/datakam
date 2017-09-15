package model

import (
	"errors"
	"fmt"
	"io"

	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/endpoints"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/dynamodb"
	"github.com/aws/aws-sdk-go/service/dynamodb/dynamodbattribute"

	"github.com/guregu/dynamo"

	"github.com/graphql-go/graphql"

	"slapman/utils"
)

const (
	defaultRegion = endpoints.UsEast1RegionID
	defaultLimit  = 24
)

var (
	// define custom GraphQL ObjectType `dyDbType` for our Golang struct `DyDbItem`
	// Note that
	// - the fields in our weatherType maps with the json tags for the fields in our struct
	// - the field type matches the field type in our struct
	//weatherType = model.WeatherType

	DyDbContentType = graphql.NewObject(graphql.ObjectConfig{
		Name: "DyDbContent",
		Fields: graphql.Fields{
			"table": &graphql.Field{
				Type:        graphql.NewNonNull(graphql.String),
				Description: "The DynamoDB Table to Scan",
			},
			"items": &graphql.Field{
				Type: graphql.NewList(DyDbItemType),
			},
		},
	})

	DyDbItemType = graphql.NewObject(graphql.ObjectConfig{
		Name: "DyDbItem",
		Fields: graphql.Fields{
			"key": &graphql.Field{
				Type:        graphql.String,
				Description: "The Key of the Current Item",
			},
			"description": &graphql.Field{
				Type:        graphql.String,
				Description: "The Description of the Current Item",
			},
			"data": &graphql.Field{
				//Type:        new(DyDbItemData),
				Type:        graphql.String,
				Description: "The Data of the Current Item",
			},
		},
	})

	// tableType = graphql.NewInputObject(
	// 	graphql.InputObjectConfig{
	// 		Name: "TableType",
	// 		Fields: graphql.InputObjectConfigFieldMap{
	// 			"table": &graphql.InputObjectFieldConfig{
	// 				Type: graphql.NewNonNull(graphql.String),
	// 			},
	// 		},
	// 	},
	// )

	DyDbFields = graphql.Field{
		Type:        DyDbContentType,
		Description: "The DynamoDB Table Items",
		Args: graphql.FieldConfigArgument{
			"table": &graphql.ArgumentConfig{
				//Type: tableType,
				Type: graphql.NewNonNull(graphql.String),
			},
			"region": &graphql.ArgumentConfig{
				Type: graphql.String,
			},
			"limit": &graphql.ArgumentConfig{
				Type: graphql.Int,
			},
		},
		Resolve: ScanItems,
	}
)

type DyDbContent struct {
	Table string     `json:"table"`
	Items []DyDbItem `json:"items"`
}

type DyDbItem struct {
	Key  int          `json:"key"`
	Desc string       `json:"description"`
	Data DyDbItemData `json:"data"`
}

type DyDbItemData map[string]interface{}

func (d *DyDbItemData) Name() string {
	return "Item Data"
}

func (d *DyDbItemData) Description() string {
	return "Item Data as a Map"
}

func (d *DyDbItemData) String() string {
	var w io.Writer
	utils.EncodeJson(w, &d)
	str := fmt.Sprint(w)
	return str
}

func (d *DyDbItemData) Error() error {
	dataError := errors.New("Item Data Error")
	return dataError
}

func NewAwsSession(region string) (sess *session.Session) {
	// Create the config specifying the Region for the DynamoDB table.
	// If config.Region is not set the region must come from the shared
	// config or AWS_REGION
	awsCfg := &aws.Config{}
	if len(region) > 0 {
		awsCfg.WithRegion(region)
	}
	utils.Debugf(nil, "AWS Config: %+v", awsCfg)

	// // Create the session that an Aws Service will use
	sess = session.Must(session.NewSession(awsCfg))
	// sess = session.Must(session.NewSessionWithOptions(session.Options{
	// 	SharedConfigState: session.SharedConfigEnable,
	// }))
	utils.Debugf(nil, "AWS Session: %+v", sess)

	return
}

// ScanItems: Scan DynamoDB Items
func ScanItems(p graphql.ResolveParams) (interface{}, error) {

	dynamo.New(session.New(), &aws.Config{Region: aws.String("us-west-2")})

	// Retrieve the Current Context
	ctx := p.Context

	table, ok := p.Args["table"].(string)
	if !ok {
		tblError := errors.New("No Tables have been specified")
		utils.Errorf(nil, "ERROR:::: %+v", tblError)
		return nil, tblError
	}

	limit, ok := p.Args["limit"].(int64)
	if !ok {
		utils.Warnf(nil, "WARNING:::: Using Default Limit of: +%v", defaultLimit)
		limit = defaultLimit
	}

	// Grab Region from Context
	region, ok := p.Args["region"].(string)
	if !ok {
		utils.Warnf(nil, "WARNING:::: Using Default AWS Region: +%v", defaultRegion)
		region = defaultRegion
	}

	// Create the session that the DynamoDB service will use
	sess := NewAwsSession(region)

	// Create the DynamoDB service client to make the query request with.
	svc := dynamodb.New(sess)

	// Build the query input parameters
	params := &dynamodb.ScanInput{
		TableName: aws.String(table),
	}
	if limit > 0 {
		params.Limit = aws.Int64(limit)
	}
	utils.Debugf(nil, "Params: %+v", params)

	// Make the DynamoDB Query API call
	result, err := svc.ScanWithContext(ctx, params)
	if err != nil {
		scanError := errors.New("Failed to make DynamoDB Query API call")
		utils.Errorf(nil, "ERROR:::: %+v", err)
		return nil, scanError
	}

	var items []DyDbItem
	// Unmarshal the Items field in the result value to the Item Go type.
	err = dynamodbattribute.UnmarshalListOfMaps(result.Items, &items)
	if err != nil {
		unmarshalError := errors.New("Failed to unmarshal Query result items")
		utils.Errorf(nil, "ERROR:::: %+v", unmarshalError)
		return nil, unmarshalError
	}

	content := DyDbContent{
		table,
		items,
	}

	return content, nil
}
