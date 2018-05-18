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

type RealThing struct {
	thing Thing `json:"thing"`
	data []Datum `json:"data"`
}

func (rt *RealThing) newThing(userID, name string) *RealThing {
	rt.thing = Thing{
		ThingID: utils.GenerateUUID(),
		UserID: userID,
		Name: name,
		Version: defaultVersion,
		Score: defaultScore,
		CreatedAt: time.Now().String(),
		UpdatedAt: time.Now().String(),
	}
	return rt
}

func (rt *RealThing) withDatum(thingID, key, value string) *RealThing {
	rt.data = append(rt.data, Datum{
		DataID: utils.GenerateUUID(),
		ThingID: thingID,
		Key: key,
		Value: value,
	})
	return rt
}

var (
	thing_logger = utils.NewLogger("resolversthing")
)
