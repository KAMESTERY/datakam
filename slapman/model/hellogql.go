package model

import (
	"github.com/graphql-go/graphql"
)

var (
	HelloFields = graphql.Field{
		Type:        graphql.String,
		Description: "Greeting or Salutation",
		Resolve:     Greet,
	}
)

func Greet(p graphql.ResolveParams) (interface{}, error) {
	return "world", nil
}
