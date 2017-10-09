package resolvers

import (
	"slapman/utils"

	"github.com/graphql-go/graphql"
)

var (
	serverip_logger = utils.NewLogger("resolversserverip")
	ServerIPFields  = graphql.Field{
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

	serverip_logger.Debugf("Response: %+v\n", response)
	return response.Ip, nil
}
