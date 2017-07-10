package main

import (
	"C"
	"encoding/json"
	"fmt"
	"log"

	"github.com/graphql-go/graphql"
	"net/http"
	"time"
	"io/ioutil"
)

var (
	schema graphql.Schema
	err error
)

func init() {
	// Schema
	fields := graphql.Fields{
		"hello": &graphql.Field{
			Type: graphql.String,
			Resolve: func(p graphql.ResolveParams) (interface{}, error) {
				return "world", nil
			},
		},
		"weather": &graphql.Field{
			Type: graphql.String,
			Resolve: func(p graphql.ResolveParams) (interface{}, error) {
				client := &http.Client{
					Timeout: time.Second * 10,
				}
				responseRaw, err := client.Get("http://samples.openweathermap.org/data/2.5/weather?id=2172797&appid=b1b15e88fa797225412429c1c50c122a1")
				if err != nil {
					return nil, err
				}

				respBytes, err := ioutil.ReadAll(responseRaw.Body)
				if err != nil {
					return nil, err
				}

				response := string(respBytes)

				fmt.Printf("Weather Response: %+v\n", response)

				return response, nil
			},
		},
	}
	rootQuery := graphql.ObjectConfig{Name: "RootQuery", Fields: fields}
	schemaConfig := graphql.SchemaConfig{Query: graphql.NewObject(rootQuery)}
	schema, err = graphql.NewSchema(schemaConfig)
	if err != nil {
		log.Fatalf("failed to create new schema, error: %v", err)
	}
}

//export ExecuteQuery
func ExecuteQuery(query *C.char) (jsonString *C.char) {
	//// Query
	//query := `
	//	{
	//		hello
	//	}
	//`
	params := graphql.Params{Schema: schema, RequestString: C.GoString(query)}
	r := graphql.Do(params)
	if len(r.Errors) > 0 {
		log.Fatalf("failed to execute graphql operation, errors: %+v", r.Errors)
	}
	rJSON, _ := json.Marshal(r)
	fmt.Printf("%s \n", rJSON) // {“data”:{“hello”:”world”}}
	jsonString = C.CString(string(rJSON))
	return
}

func main() {
}
