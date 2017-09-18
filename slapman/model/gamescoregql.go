package model

import (
	"slapman/utils"

	"github.com/graphql-go/graphql"
)

var (
	GameScoreType = graphql.NewObject(graphql.ObjectConfig{
		Name: "GameScore",
		Fields: graphql.Fields{
			"table": &graphql.Field{
				Type:        graphql.NewNonNull(graphql.String),
				Description: "The DynamoDB Table to Scan",
			},
			"rows": &graphql.Field{
				Type: graphql.NewList(GameScoreRowType),
			},
		},
	})

	GameScoreRowType = graphql.NewObject(graphql.ObjectConfig{
		Name: "GameScoreRow",
		Fields: graphql.Fields{
			"UserId": &graphql.Field{
				Type:        graphql.Int,
				Description: "The UserID of the Current GameScore",
			},
			"GameTitle": &graphql.Field{
				Type:        graphql.String,
				Description: "The GameTitle of the Current GameScore",
			},
			"TopScore": &graphql.Field{
				Type:        graphql.Int,
				Description: "The TopScore of the Current GameScore",
			},
		},
	})

	GameScoreFields = graphql.Field{
		Type:        GameScoreType,
		Description: "The DynamoDB Table Items",
		Args: graphql.FieldConfigArgument{
			"region": &graphql.ArgumentConfig{
				Type: graphql.String,
			},
			"limit": &graphql.ArgumentConfig{
				Type: graphql.Int,
			},
		},
		Resolve: func(p graphql.ResolveParams) (interface{}, error) {
			return utils.DynaResolveScanItems(p, "GameScores")
		},
	}
)
