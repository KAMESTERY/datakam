package resolvers

import (
	"slapman/utils"

	"github.com/graphql-go/graphql"
)

var (
	user_logger      = utils.NewLogger("resolvershello")
	UserCreateFields = graphql.Field{
		Type:        graphql.String,
		Description: "Greeting or Salutation",
		Resolve: func(p graphql.ResolveParams) (interface{}, error) {
			user_logger.Debugf("Greeting %+v", p)
			return "world", nil
		},
	}
)
