package utils

import (
	"bytes"
	"encoding/json"
	"net/http"
)

// Rendering START

func RenderCsv(lst []string) string {
	var csv bytes.Buffer
	for index, item := range lst {
		csv.WriteString(item)
		if index < (len(lst) - 1) {
			csv.WriteString(",")
		}
	}
	return csv.String()
}

func RenderJsonString(data interface{}) (jsonString string) {
	jsonBytes, err := json.Marshal(data)
	if err != nil {
		Errorf(nil, "ERROR:::: %+v", err.Error())
	}
	jsonString = string(jsonBytes)
	return
}

// RenderJSON Renders Data as JSON with HTTP Ok
func RenderJSON(w http.ResponseWriter, r *http.Request, data interface{}) {
	RenderJSONWithCode(w, r, data, http.StatusOK)
}

// RenderJSONWithCode Renders Data as JSON with Specified HTTP Code
func RenderJSONWithCode(w http.ResponseWriter, r *http.Request, data interface{}, code int) {
	w.Header().Set("Content-Type", "application/json; charset=UTF-8")
	defer w.WriteHeader(code)
	if err := EncodeJson(w, data); err != nil {
		Errorf(r, "ERROR:::: %+v", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}

// Rendering STOP
