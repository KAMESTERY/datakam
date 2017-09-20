package model

import (
	"errors"
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

	GameScoreScanFields = graphql.Field{
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
		Type:        GameScoreType,
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

			var data map[string]interface{}

			userId, _ := p.Args["userId"].(string)
			if len(userId) == 0 {
				return nil, errors.New("An existing UserId needs to be provided")
			}
			utils.Debugf(nil, "Updating GameScore with UserId: %+v", userId)

			if gameTitle, ok := p.Args["gameTitle"].(string); ok {
				data["GameTitle"] = gameTitle
				utils.Debugf(nil, "Updating GameScore with GameTitle: %+v", gameTitle)
			}

			topScore, err := utils.ParseInt64(p.Args["topScore"])
			if err == nil { // There has to be a topScore
				data["TopScore"] = topScore
				utils.Debugf(nil, "Updating GameScore with TopScore: %+v", topScore)
			}

			return utils.DynaResolveUpdateItem(p, "GameScores", "UserId", userId, data)
		},
	}
)
