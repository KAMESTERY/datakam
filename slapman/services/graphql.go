package services

import (
	//"C"
	"encoding/json"

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
		"hello":   &model.HelloFields,
		"weather": &model.WeatherFields,
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

////export ExecuteQuery
// func ExecuteQuery(query string) (jsonString string) {
// 	params := graphql.Params{Schema: schema, RequestString: query}
// 	r := graphql.Do(params)
// 	if len(r.Errors) > 0 {
// 		utils.Fatalf(nil, "failed to execute graphql operation, errors: %+v", r.Errors)
// 	}
// 	rJSON, _ := json.Marshal(r)
// 	utils.Debugf(nil, "%s \n", rJSON)
// 	jsonString = string(rJSON)
// 	return
// }

//export ExecuteQuery
//func ExecuteQuery(query *C.char) (jsonString *C.char) {
//	params := graphql.Params{Schema: schema, RequestString: C.GoString(query)}
//	r := graphql.Do(params)
//	if len(r.Errors) > 0 {
//		log.Fatalf("failed to execute graphql operation, errors: %+v", r.Errors)
//	}
//	rJSON, _ := json.Marshal(r)
//	fmt.Printf("%s \n", rJSON)
//	jsonString = C.CString(string(rJSON))
//	return
//}

// Got it from here: https://gist.github.com/hyg/9c4afcd91fe24316cbf0
//func OpenBrowser(url string) {
//	var err error
//
//	switch runtime.GOOS {
//	case "linux":
//		err = exec.Command("xdg-open", url).Start()
//	case "windows":
//		err = exec.Command("rundll32", "url.dll,FileProtocolHandler", url).Start()
//	case "darwin":
//		err = exec.Command("open", url).Start()
//	default:
//		err = fmt.Errorf("unsupported platform")
//	}
//	if err != nil {
//		log.Fatal(err)
//	}
//
//}

func executeQuery(w http.ResponseWriter, r *http.Request, query string) {
	params := graphql.Params{Schema: schema, RequestString: query}
	response := graphql.Do(params)
	if len(response.Errors) > 0 {
		utils.Fatalf(nil, "failed to execute graphql operation, errors: %+v", response.Errors)
		utils.RenderJSONWithCode(w, r, response.Errors, http.StatusInternalServerError)
		return
	}
	utils.Debugf(nil, "%s \n", response)
	utils.RenderJSON(w, r, response)
}

type GqlRequest struct {
	Query string
}

func HandleGqlRequest(w http.ResponseWriter, r *http.Request) {

	switch r.Method {
	case "GET":
		query := mux.Vars(r)["query"]

		utils.Debugf(r, "Query: %+v", query)

		executeQuery(w, r, query)
	case "POST":
		var gqlReq GqlRequest

		// decode into GqlRequest struct
		err := json.NewDecoder(r.Body).Decode(&gqlReq)
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
