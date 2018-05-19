package resolvers

import (
	"fmt"
	"reflect"
	"slapman/utils"
	"slapman/utils/group"
	"time"

	"github.com/graphql-go/graphql"
)

const (
	thingsTable    = "Things"
	dataTable      = "Data"
	defaultVersion = 0
	defaultScore   = 0
)

type Thing struct {
	ThingID   string `json:"ThingID"`
	UserID    string `json:"UserID"`
	Name      string `json:"Name"`
	Version   int    `json:"Version"`
	Score     int    `json:"Score"`
	CreatedAt string `json:"CreatedAt"`
	UpdatedAt string `json:"UpdatedAt"`
}

type Datum struct {
	DataID  string `json:"DataID"`
	ThingID string `json:"ThingID"`
	Key     string `json:"Key"`
	Value   string `json:"Value"`
}

type ActualThing struct {
	thing  Thing   `json:"thing"`
	data   []Datum `json:"data"`
	errors []error
}

func (athg *ActualThing) Ok() (isOk bool, err error) {
	isOk = len(athg.errors) == 0
	if !isOk {
		err = fmt.Errorf("VALIDATION_FAILED::::%+v", athg.errors)
		thing_logger.Errorf("THING_ERROR:::: %+v", err)
	}
	return
}

func (athg *ActualThing) SaveThingData(p graphql.ResolveParams) (status string, err error) {

	if _, err = athg.Ok(); err != nil {
		status = "Could not Persist Thing / Data"
		return
	}

	var g group.Group
	{
		// Persist Thing
		g.Add(func() (err error) {
			_, err = utils.DynaResolvePutItem(p, thingsTable, athg.thing)
			return
		}, func(err error) {
			thing_logger.Errorf("ERROR:::: %+v", err)
		})

		// Persist Datum
		g.Add(func() (err error) {
			for _, datum := range athg.data {
				_, err = utils.DynaResolvePutItem(p, dataTable, datum)
			}
			return
		}, func(err error) {
			thing_logger.Errorf("ERROR:::: %+v", err)
		})
	}

	err = g.Run()
	if err != nil {
		thing_logger.Errorf("ERROR:::: %+v", err)
		return
	}

	status = "success"

	return
}

func (athg *ActualThing) newThing(userID, name string) *ActualThing {

	if userID == "" || name == "" {
		athg.errors = append(athg.errors, fmt.Errorf("Both a UserID and a Name are required! [User:%+v, Name:%+v]", userID, name))
		return athg
	}

	athg.thing = Thing{
		ThingID:   utils.GenerateUUID(),
		UserID:    userID,
		Name:      name,
		Version:   defaultVersion,
		Score:     defaultScore,
		CreatedAt: time.Now().String(),
		UpdatedAt: time.Now().String(),
	}
	return athg
}

func (athg *ActualThing) withDatum(key, value string) *ActualThing {

	if len(athg.errors) != 0 {
		athg.errors = append(athg.errors, fmt.Errorf("Cannot add a Datum to a Thing that is not Ok!"))
		return athg
	}

	if key == "" || value == "" {
		athg.errors = append(athg.errors, fmt.Errorf("Both a Key and a Value are required! [%+v:%+v]", key, value))
		return athg
	}

	athg.data = append(athg.data, Datum{
		DataID:  utils.GenerateUUID(),
		ThingID: athg.thing.ThingID,
		Key:     key,
		Value:   value,
	})
	return athg
}

var (
	thing_logger = utils.NewLogger("resolversthing")

	// NewThingFields represents the parameters and the resolver function to create a New Thing and its Data
	NewThingFields = graphql.Field{
		Type:        graphql.String,
		Description: "Creates a New Thing and its Data",
		Args: graphql.FieldConfigArgument{
			"region": &graphql.ArgumentConfig{
				Type: graphql.String,
			},
			"limit": &graphql.ArgumentConfig{
				Type: graphql.Int,
			},
			"token": &graphql.ArgumentConfig{
				Type:        graphql.NewNonNull(graphql.String),
				Description: "The JWT Token",
			},
			"userID": &graphql.ArgumentConfig{
				Type:        graphql.NewNonNull(graphql.String),
				Description: "The UserID",
			},
			"name": &graphql.ArgumentConfig{
				Type:        graphql.NewNonNull(graphql.String),
				Description: "The Name of the Thing",
			},
			"parameters": &graphql.ArgumentConfig{
				Type:        graphql.NewList(utils.DynaParamInputType),
				Description: "The DynamoDB Parameters to Use",
			},
		},
		Resolve: func(p graphql.ResolveParams) (interface{}, error) {

			err := utils.ValidateRsa256JwtTokenInParams(p.Args)
			if err != nil {
				return nil, err
			}

			userID := p.Args["userID"].(string)
			name := p.Args["name"].(string)

			params, ok := p.Args["parameters"].([]interface{})
			if !ok {
				thing_logger.Debugf("PARAMETERS TYPE: %+v", reflect.TypeOf(p.Args["parameters"]))
				thingError := fmt.Errorf("Could not Create the Thing with the Provided Arguments: %+v", p.Args)
				thing_logger.Errorf("ERROR:::: %+v", thingError)
				return nil, thingError
			}
			thing_logger.Debugf("Data Params: %+v", params)

			athg := ActualThing{}
			athg.newThing(userID, name)

			for _, param := range params {

				p := param.(map[string]interface{})
				key := p["field"].(string)
				value := p["value"].(string)

				thing_logger.Debugf("Adding Param [%+v:%+v] to Thing [%+v]", key, value, name)

				athg.withDatum(key, value)
			}

			return athg.SaveThingData(p)
		},
	}
)
