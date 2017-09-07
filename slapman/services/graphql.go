package services

import (
	"github.com/graphql-go/graphql"
	"net/http"

	"github.com/gorilla/mux"

	"slapman/model"
	"slapman/utils"
)

var (
	schema graphql.Schema
	err    error

	// Schema
	fields = graphql.Fields{
		"hello":    &model.HelloFields,
		"weather":  &model.WeatherFields,
		"serverip": &model.ServerIPFields,
	}

	rootQuery    = graphql.ObjectConfig{Name: "RootQuery", Fields: fields}
	schemaConfig = graphql.SchemaConfig{Query: graphql.NewObject(rootQuery)}
)

func init() {
	schema, err = graphql.NewSchema(schemaConfig)
	if err != nil {
		utils.Fatalf(nil, "failed to create new schema, error: %v", err)
	}
}

func executeQuery(w http.ResponseWriter, r *http.Request, query string) {
	params := graphql.Params{Schema: schema, RequestString: query}
	response := graphql.Do(params)
	if len(response.Errors) > 0 {
		utils.Fatalf(nil, "failed to execute graphql operation, errors: %+v", response.Errors)
		utils.RenderJSONWithCode(w, r, response.Errors, http.StatusInternalServerError)
		return
	}
	utils.Debugf(r, "%s \n", response)
	utils.RenderJSON(w, r, response)
}

func HandleGqlRequest(w http.ResponseWriter, r *http.Request) {

	switch r.Method {
	case "GET":
		query := mux.Vars(r)["query"]

		utils.Debugf(r, "Query: %+v", query)

		executeQuery(w, r, query)
	case "POST":
		var gqlReq struct {
			Query string `json:"query"`
		}

		// decode into GqlRequest struct
		err := utils.DecodeJson(r.Body, &gqlReq)
		if err != nil {
			utils.RenderJSONWithCode(w, r, err.Error(), http.StatusInternalServerError)
			return
		}

		utils.Debugf(r, "Query: %+v", gqlReq.Query)

		executeQuery(w, r, gqlReq.Query)
	default:
		warnStruct := struct {
			Msg string
		}{"Bad Request"}
		utils.Warnf(r, "ERROR:::: %+v", warnStruct)
		utils.RenderJSONWithCode(w, r, warnStruct, http.StatusBadRequest)
	}
}
