package resolvers

import (
	"github.com/graphql-go/graphql"

	"slapman/utils"
)

type Weather struct {
	Location    string  `json:"location"`
	Description string  `json:"description"`
	Temp        float64 `json:"temp"`
	Pressure    int64   `json:"pressure"`
	Humidity    int64   `json:"humidity"`
	Speed       float64 `json:"speed"`
	Deg         float64 `json:"deg"`
}

var (
	weather_logger = utils.NewLogger("resolversweather")
	// define custom GraphQL ObjectType `weatherType` for our Golang struct `weatherStruct`
	// Note that
	// - the fields in our weatherType maps with the json tags for the fields in our struct
	// - the field type matches the field type in our struct
	//weatherType = model.WeatherType

	WeatherType = graphql.NewObject(graphql.ObjectConfig{
		Name: "Weather",
		Fields: graphql.Fields{
			"location": &graphql.Field{
				Type:        graphql.NewNonNull(graphql.String),
				Description: "The location for which to retrieve the Weather information",
			},
			"description": &graphql.Field{
				Type: graphql.String,
			},
			"temp": &graphql.Field{
				Type: graphql.Float,
			},
			"pressure": &graphql.Field{
				Type: graphql.Int,
			},
			"humidity": &graphql.Field{
				Type: graphql.Int,
			},
			"speed": &graphql.Field{
				Type: graphql.Float,
			},
			"deg": &graphql.Field{
				Type: graphql.Float,
			},
		},
	})

	// locationType = graphql.NewInputObject(
	// 	graphql.InputObjectConfig{
	// 		Name: "LocationType",
	// 		Fields: graphql.InputObjectConfigFieldMap{
	// 			"location": &graphql.InputObjectFieldConfig{
	// 				Type: graphql.NewNonNull(graphql.String),
	// 			},
	// 		},
	// 	},
	// )

	WeatherFields = graphql.Field{
		Type:        WeatherType,
		Description: "The Weather",
		Args: graphql.FieldConfigArgument{
			"location": &graphql.ArgumentConfig{
				//Type: locationType,
				Type: graphql.NewNonNull(graphql.String),
			},
		},
		Resolve: FetchWeather,
	}
)

func FetchWeather(params graphql.ResolveParams) (interface{}, error) {

	locationQuery, isOk := params.Args["location"].(string)

	if isOk {
		weather_logger.Debugf("Is OK: %+v\n", isOk)
	}

	var response struct {
		Weather []struct {
			Main        string `json:"main"`
			Description string `json:"description"`
		} `json:"weather"`
		Main struct {
			Temp     float64 `json:"temp"`
			Pressure int64   `json:"pressure"`
			Humidity int64   `json:"humidity"`
			TempMin  float64 `json:"temp_min"`
			TempMax  float64 `json:"temp_max"`
		} `json:"main"`
		Wind struct {
			Speed float64 `json:"speed"`
			Deg   float64 `json:"deg"`
		} `json:"wind"`
	}

	//TODO: Eventually Check Authentication from Context
	err := utils.GetJson(
		params.Context,
		"http://samples.openweathermap.org/data/2.5/weather?id=2172797&appid=b1b15e88fa797225412429c1c50c122a1",
		&response,
	)
	if err != nil {
		return nil, err
	}

	weather_logger.Debugf("Response: %+v\n", response)

	weather := Weather{
		Location:    locationQuery,
		Description: response.Weather[0].Description,
		Temp:        response.Main.Temp,
		Pressure:    response.Main.Pressure,
		Humidity:    response.Main.Humidity,
		Speed:       response.Wind.Speed,
		Deg:         response.Wind.Deg,
	}

	weather_logger.Debugf("Weather: %+v\n", weather)

	// return the new Weather object that we supposedly save to DB
	// Note here that
	// - we are returning a `Weather` struct instance here
	// - we previously specified the return Type to be `todoType`
	// - `Weather` struct maps to `weatherType`, as defined in `weatherType` ObjectConfig`
	return weather, nil
}