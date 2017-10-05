package model

import (
	"fmt"
	"reflect"

	"slapman/utils"

	"github.com/graphql-go/graphql"
)

var (
	// GameScoreListType represents a list of GameScores
	GameScoreListType = graphql.NewObject(graphql.ObjectConfig{
		Name: "GameScoreList",
		Fields: graphql.Fields{
			"table": &graphql.Field{
				Type:        graphql.NewNonNull(graphql.String),
				Description: "The DynamoDB Table to Scan",
			},
			"count": &graphql.Field{
				Type:        graphql.NewNonNull(graphql.Int),
				Description: "The DynamoDB Table Rows Count",
			},
			"rows": &graphql.Field{
				Type:        graphql.NewList(GameScoreRowType),
				Description: "The DynamoDB Table Rows",
			},
		},
	})

	GameScorePageListType = graphql.NewObject(graphql.ObjectConfig{
		Name: "GameScorePageList",
		Fields: graphql.Fields{
			"table": &graphql.Field{
				Type:        graphql.NewNonNull(graphql.String),
				Description: "The DynamoDB Table to Scan",
			},
			"page": &graphql.Field{
				Type:        graphql.NewNonNull(graphql.Int),
				Description: "The DynamoDB Table Rows Count",
			},
			"count": &graphql.Field{
				Type:        graphql.NewNonNull(graphql.Int),
				Description: "The DynamoDB Table Rows Count",
			},
			"rows": &graphql.Field{
				Type:        graphql.NewList(GameScoreRowType),
				Description: "The DynamoDB Table Rows",
			},
		},
	})

	GameScoreRowType = graphql.NewObject(graphql.ObjectConfig{
		Name: "GameScoreRow",
		Fields: graphql.Fields{
			"UserId": &graphql.Field{
				Type:        graphql.String,
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

	GameScoreUpdateType = graphql.NewObject(graphql.ObjectConfig{
		Name: "GameScoreUpdate",
		Fields: graphql.Fields{
			"table": &graphql.Field{
				Type:        graphql.NewNonNull(graphql.String),
				Description: "The DynamoDB Table to Scan",
			},
			"update": &graphql.Field{
				Type: GameScoreRowType,
			},
		},
	})

	GameQueryFields = graphql.Field{
		Type:        GameScoreListType,
		Description: "The DynamoDB Table Query Items",
		Args:        utils.DynaQueryArgs,
		Resolve: func(p graphql.ResolveParams) (interface{}, error) {

			table, _ := p.Args["table"].(string)
			index, _ := p.Args["index"].(string)
			params, ok := p.Args["parameters"].([]interface{})
			if !ok {
				utils.Debugf(nil, "PARAMETERS TYPE: %+v", reflect.TypeOf(p.Args["parameters"]))
				queryError := fmt.Errorf("Could not Execute the Query with the Provided Arguments: %+v", p.Args)
				utils.Errorf(nil, "ERROR:::: %+v", queryError)
				return nil, queryError
			}

			utils.Debugf(nil, "QUERY PARAMS: %+v", params)

			queryBuilder := utils.DynaQueryDsl(p.Context, table, index).Build(params)

			limit, ok := p.Args["limit"].(int)
			if ok && limit > 0 {
				queryBuilder.WithLimit(limit)
				utils.Debugf(nil, "Limiting Query Results Count to: %+v", limit)
			}

			queryInput, err := queryBuilder.AsInput()
			if err != nil {
				return nil, err
			}

			count, rows, err := utils.DynaResolveQuery(p, queryInput)
			if err != nil {
				return nil, err
			}
			return struct {
				Table string      `json:"table"`
				Count int         `json:"count"`
				Rows  interface{} `json:"rows"`
			}{
				"GameScores",
				count,
				rows,
			}, nil
		},
	}

	GameScoreScanFields = graphql.Field{
		Type:        GameScoreListType,
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
			count, rows, err := utils.DynaResolveScanItems(p, "GameScores")
			if err != nil {
				return nil, err
			}
			return struct {
				Table string      `json:"table"`
				Count int         `json:"count"`
				Rows  interface{} `json:"rows"`
			}{
				"GameScores",
				count,
				rows,
			}, nil
		},
	}

	GameScoreScanPagesFields = graphql.Field{
		Type:        GameScorePageListType,
		Description: "The DynamoDB Table Items",
		Args: graphql.FieldConfigArgument{
			"page": &graphql.ArgumentConfig{
				Type: graphql.Int,
			},
			"region": &graphql.ArgumentConfig{
				Type: graphql.String,
			},
			"limit": &graphql.ArgumentConfig{
				Type: graphql.Int,
			},
		},
		Resolve: func(p graphql.ResolveParams) (interface{}, error) {
			count, rows, err := utils.DynaResolveScanPages(p, "GameScores")
			if err != nil {
				return nil, err
			}
			return struct {
				Table string      `json:"table"`
				Count int         `json:"count"`
				Rows  interface{} `json:"rows"`
			}{
				"GameScores",
				count,
				rows,
			}, nil
		},
	}

	GameScorePutFields = graphql.Field{
		Type:        graphql.String,
		Description: "The DynamoDB Table Items",
		Args: graphql.FieldConfigArgument{
			"userId": &graphql.ArgumentConfig{
				Type: graphql.String,
			},
			"gameTitle": &graphql.ArgumentConfig{
				Type: graphql.NewNonNull(graphql.String),
			},
			"topScore": &graphql.ArgumentConfig{
				Type: graphql.NewNonNull(graphql.String),
			},
		},
		Resolve: func(p graphql.ResolveParams) (interface{}, error) {

			utils.Debugf(nil, "Put Args: %+v", p.Args)

			gameScore := struct {
				UserId    string `json:"UserId"`
				GameTitle string `json:"GameTitle"`
				TopScore  int64  `json:"TopScore"`
			}{}

			userId, _ := p.Args["userId"].(string) // It is ok to ignore the ok/notok since userId is only optional
			if len(userId) == 0 {
				userId = utils.GenerateUUID()
			}
			gameScore.UserId = userId
			utils.Debugf(nil, "Putting GameScore using UserId: %+v", userId)

			gameTitle, ok := p.Args["gameTitle"].(string)
			if ok {
				gameScore.GameTitle = gameTitle
				utils.Debugf(nil, "Putting GameScore using GameTitle: %+v", gameTitle)
			}

			topScore, err := utils.ParseInt64(p.Args["topScore"])
			if err != nil { // There has to be a topScore
				return nil, err
			}
			gameScore.TopScore = topScore
			utils.Debugf(nil, "Putting GameScore using TopScore: %+v", topScore)

			return utils.DynaResolvePutItem(p, "GameScores", gameScore)
		},
	}

	GameScoreUpdateFields = graphql.Field{
		Type:        GameScoreUpdateType,
		Description: "The DynamoDB Table Items",
		Args: graphql.FieldConfigArgument{
			"userId": &graphql.ArgumentConfig{
				Type: graphql.NewNonNull(graphql.String),
			},
			"gameTitle": &graphql.ArgumentConfig{
				Type: graphql.NewNonNull(graphql.String),
			},
			"topScore": &graphql.ArgumentConfig{
				Type: graphql.NewNonNull(graphql.String),
			},
		},
		Resolve: func(p graphql.ResolveParams) (interface{}, error) {

			utils.Debugf(nil, "Put Args: %+v", p.Args)

			keyData := make(map[string]interface{})
			data := make(map[string]interface{})

			if userId, ok := p.Args["userId"].(string); ok {
				keyData["UserId"] = userId
				utils.Debugf(nil, "Updating GameScore with UserId: %+v", userId)
			}

			if gameTitle, ok := p.Args["gameTitle"].(string); ok {
				keyData["GameTitle"] = gameTitle
				utils.Debugf(nil, "Updating GameScore with GameTitle: %+v", gameTitle)
			}

			topScore, err := utils.ParseInt64(p.Args["topScore"])
			if err == nil { // There has to be a topScore
				data["TopScore"] = topScore
				utils.Debugf(nil, "Updating GameScore with TopScore: %+v", topScore)
			}

			updatedItem, err := utils.DynaResolveUpdateItem(p, "GameScores", keyData, data)
			if err != nil {
				return nil, err
			}
			return struct {
				Table  string      `json:"table"`
				Update interface{} `json:"update"`
			}{
				"GameScore",
				updatedItem,
			}, nil
		},
	}
)
