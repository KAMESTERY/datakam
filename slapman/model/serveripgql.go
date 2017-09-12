package model

import (
	"github.com/graphql-go/graphql"
	"slapman/utils"
)

var (
	ServerIPFields = graphql.Field{
		Type:        graphql.String,
		Description: "The Current IP of the Lambda Function",
		Resolve:     GetServerIP,
	}
)

func GetServerIP(p graphql.ResolveParams) (interface{}, error) {

	var response struct {
		Ip string `json:"ip"`
	}

	//TODO: Eventually Check Authentication from Context
	err := utils.GetJson(
		p.Context,
		"https://api.ipify.org/?format=json",
		&response,
	)
	if err != nil {
		return nil, err
	}

	utils.Debugf(nil, "Response: %+v\n", response)
	return response.Ip, nil
}
