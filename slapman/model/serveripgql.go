package model

import (
	"context"
	"github.com/graphql-go/graphql"
	"slapman/utils"
	"time"
)

var (
	ServerIPFields = graphql.Field{
		Type:        graphql.String,
		Description: "The Current IP of the Lambda Function",
		Resolve:     GetServerIP,
	}
)

func GetServerIP(p graphql.ResolveParams) (interface{}, error) {

	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	defer cancel()

	var response struct {
		Ip string `json:"ip"`
	}

	err := utils.GetJson(
		ctx,
		"https://api.ipify.org/?format=json",
		&response,
	)
	if err != nil {
		return nil, err
	}

	utils.Debugf(nil, "Response: %+v\n", response)
	return response.Ip, nil
}
