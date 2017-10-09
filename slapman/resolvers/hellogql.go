package resolvers

import (
	"slapman/utils"

	"github.com/graphql-go/graphql"
)

var (
	hello_logger = utils.NewLogger("resolvershello")
	HelloFields  = graphql.Field{
		Type:        graphql.String,
		Description: "Greeting or Salutation",
		Resolve:     Greet,
	}
)

func Greet(p graphql.ResolveParams) (interface{}, error) {
	hello_logger.Debugf("Greeting %+v", p)
	return "world", nil
}
