package resolvers

import (
	"slapman/utils"
	"time"
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
	thing Thing `json:"thing"`
	data []Datum `json:"data"`
}

func (athg *ActualThing) newThing(userID, name string) *ActualThing {
	athg.thing = Thing{
		ThingID: utils.GenerateUUID(),
		UserID: userID,
		Name: name,
		Version: defaultVersion,
		Score: defaultScore,
		CreatedAt: time.Now().String(),
		UpdatedAt: time.Now().String(),
	}
	return athg
}

func (athg *ActualThing) withDatum(thingID, key, value string) *ActualThing {
	athg.data = append(athg.data, Datum{
		DataID: utils.GenerateUUID(),
		ThingID: thingID,
		Key: key,
		Value: value,
	})
	return athg
}

var (
	thing_logger = utils.NewLogger("resolversthing")
)
