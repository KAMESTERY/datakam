package services

import (
	"context"
	"net/http"
	"time"

	"github.com/graphql-go/graphql"

	"github.com/gorilla/mux"

	"slapman/model"
	"slapman/utils"
)

var (
	schema graphql.Schema
	err    error

	// Schema

	queryFields = graphql.Fields{
		"hello":              &model.HelloFields,
		"weather":            &model.WeatherFields,
		"serverip":           &model.ServerIPFields,
		"gameScoreScan":      &model.GameScoreScanFields,
		"gameScoreScanPages": &model.GameScoreScanPagesFields,
		"gameScoreQuery":     &model.GameQueryFields,
	}
	rootQuery = graphql.ObjectConfig{Name: "RootQuery", Fields: queryFields}

	mutationFields = graphql.Fields{
		"gameScorePut":    &model.GameScorePutFields,
		"gameScoreUpdate": &model.GameScoreUpdateFields,
	}
	rootMutation = graphql.ObjectConfig{Name: "RootMutation", Fields: mutationFields}

	schemaConfig = graphql.SchemaConfig{
		Query:    graphql.NewObject(rootQuery),
		Mutation: graphql.NewObject(rootMutation),
	}
)

func init() {
	schema, err = graphql.NewSchema(schemaConfig)
	if err != nil {
		utils.Fatalf(nil, "failed to create new schema, error: %v", err)
	}
}

func executeQuery(ctx context.Context, w http.ResponseWriter, r *http.Request, query string) {
	respChan := make(chan interface{}, 1)
	go func() {
		params := graphql.Params{
			Schema:        schema,
			RequestString: query,
			Context:       ctx,
		}
		response := graphql.Do(params)
		if len(response.Errors) > 0 {
			utils.Fatalf(nil, "failed to execute graphql operation, errors: %+v", response.Errors)
			respChan <- response.Errors
		} else {
			utils.Debugf(r, "%s \n", response)
			respChan <- response
		}
	}()
	select {
	case resp := <-respChan:
		utils.Debugf(r, "%s \n", resp)
		utils.RenderJSON(w, r, resp)
	case <-ctx.Done():
		utils.Fatalf(nil, "ERROR:::: %+v", ctx.Err())
		utils.RenderJSONWithCode(w, r, ctx.Err(), http.StatusInternalServerError)
	}
}

func HandleGqlRequest(w http.ResponseWriter, r *http.Request) {

	ctx, cancel := context.WithTimeout(r.Context(), time.Second)
	defer cancel()

	switch r.Method {
	case "OPTIONS":
		// CORS Headers
		utils.CorsHeaders(w)
		utils.RenderJSON(w, r, struct{}{})
	case "GET":
		query := mux.Vars(r)["query"]
		//query := r.URL.Query().Get("query")

		utils.Debugf(r, "Query: %+v", query)

		// CORS Headers
		utils.CorsHeaders(w)

		executeQuery(ctx, w, r, query)
	case "POST":
		// CORS Headers
		utils.CorsHeaders(w)

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

		executeQuery(ctx, w, r, gqlReq.Query)
	default:
		warnStruct := struct {
			Msg string
		}{"Bad Request"}
		utils.Warnf(r, "ERROR:::: %+v", warnStruct)
		utils.RenderJSONWithCode(w, r, warnStruct, http.StatusBadRequest)
	}
}
