package services

import (
	"context"
	"fmt"
	"net/http"
	"time"

	"github.com/aws/aws-lambda-go/lambda"
	_ "github.com/aws/aws-lambda-go/lambda"

	"github.com/graphql-go/graphql"

	"github.com/gorilla/mux"

	"slapman/resolvers"
	"slapman/utils"
)

// http://docs.aws.amazon.com/lambda/latest/dg/limits.html
const maxExecutionDuration = 300 * time.Second

var (
	graphql_logger = utils.NewLogger("servicesgraphql")

	schema graphql.Schema
	err    error

	// Schema

	queryFields = graphql.Fields{
		"hello":              &resolvers.HelloFields,
		"weather":            &resolvers.WeatherFields,
		"serverip":           &resolvers.ServerIPFields,
		"gameScoreScan":      &resolvers.GameScoreScanFields,
		"gameScoreScanPages": &resolvers.GameScoreScanPagesFields,
		"gameScoreQuery":     &resolvers.GameQueryFields,
		"userLogin":          &resolvers.UserLoginFields,
	}
	rootQuery = graphql.ObjectConfig{Name: "RootQuery", Fields: queryFields}

	mutationFields = graphql.Fields{
		"gameScorePut":        &resolvers.GameScorePutFields,
		"gameScoreUpdate":     &resolvers.GameScoreUpdateFields,
		"userCreate":          &resolvers.UserCreateFields,
		"userJwtTokenRefresh": &resolvers.UserJwtTokenRefreshFields,
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
		graphql_logger.Fatalf("failed to create new schema, error: %v", err)
	}
}

func executeQuery(ctx context.Context, w http.ResponseWriter, r *http.Request, requestString string) {
	respChan := make(chan interface{}, 1)
	go func() {
		params := graphql.Params{
			Schema:        schema,
			RequestString: requestString,
			Context:       ctx,
		}
		response := graphql.Do(params)
		if len(response.Errors) > 0 {
			graphql_logger.Fatalf("failed to execute graphql operation, errors: %+v", response.Errors)
			respChan <- response.Errors
		} else {
			graphql_logger.Debugf("%s \n", response)
			respChan <- response
		}
	}()
	select {
	case resp := <-respChan:
		graphql_logger.Debugf("%s \n", resp)
		utils.RenderJSON(w, r, resp)
	case <-ctx.Done():
		graphql_logger.Fatalf("ERROR:::: %+v", ctx.Err())
		utils.RenderJSONWithCode(w, r, ctx.Err(), http.StatusInternalServerError)
	}
}

type GqlRequest struct {
	OperationName string                 `json:"operationName"`
	Query         string                 `json:"query"`
	Variables     map[string]interface{} `json:"variables"`
}

func HandleGqlRequest(w http.ResponseWriter, r *http.Request) {

	ctx, cancel := context.WithTimeout(r.Context(), maxExecutionDuration)
	defer cancel()

	switch r.Method {
	case "OPTIONS":
		// CORS Headers
		utils.CorsHeaders(w)
		utils.RenderJSON(w, r, struct{}{})
	case "GET":
		query := mux.Vars(r)["query"]
		//query := r.URL.Query().Get("query")

		graphql_logger.Debugf("Query: %+v", query)

		// CORS Headers
		utils.CorsHeaders(w)

		executeQuery(ctx, w, r, query)
	case "POST":
		// CORS Headers
		utils.CorsHeaders(w)

		var gqlReq GqlRequest

		// decode into GqlRequest struct
		err := utils.DecodeJson(r.Body, &gqlReq)
		if err != nil {
			utils.RenderJSONWithCode(w, r, err.Error(), http.StatusInternalServerError)
			return
		}

		graphql_logger.Debugf("Query: %+v", gqlReq.Query)

		executeQuery(ctx, w, r, gqlReq.Query)
	default:
		warnStruct := struct {
			Msg string
		}{"Bad Request"}
		graphql_logger.Warnf("ERROR:::: %+v", warnStruct)
		utils.RenderJSONWithCode(w, r, warnStruct, http.StatusBadRequest)
	}
}

func lambdaGqlHandler(context context.Context, gqlReq GqlRequest) (response *graphql.Result, err error) {

	graphql_logger.Debugf("Request: %+v", gqlReq)

	requestString := gqlReq.Query
	graphql_logger.Debugf("Query: %+v", requestString)

	respChan := make(chan *graphql.Result, 1)
	go func() {
		params := graphql.Params{
			Schema:        schema,
			RequestString: requestString,
			Context:       context,
		}
		response := graphql.Do(params)
		if len(response.Errors) > 0 {
			graphql_logger.Fatalf("failed to execute graphql operation, errors: %+v", response.Errors)
			err = fmt.Errorf("ERROR:::: %+v", response.Errors)
		} else {
			graphql_logger.Debugf("%s \n", response)
		}
		respChan <- response
	}()
	select {
	case resp := <-respChan:
		response = resp
		graphql_logger.Debugf("%s \n", response)
	case <-context.Done():
		err = context.Err()
		graphql_logger.Fatalf("ERROR:::: %+v", err)
	}

	return
}

//RunLambda Starts the lambdaGqlHandler which Handles AWS Lambda GraphQL Queries and Mutions
func RunLambda() {
	lambda.Start(lambdaGqlHandler)
}
