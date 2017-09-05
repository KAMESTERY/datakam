package services

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"slapman/model"
	"slapman/utils"
	"net/http"
	"os"
)

// Translate sends submitted content for translation and returns the translated text
func Translate(w http.ResponseWriter, r *http.Request) {

	var (
		transDataIn  model.TransData
		transDataOut model.TransData
	)

	// decode into TransData struct
	err := json.NewDecoder(r.Body).Decode(&transDataIn)
	if err != nil {
		utils.Errorf(r, "ERROR:::: %+v", err)
		w.WriteHeader(http.StatusInternalServerError)
		fmt.Fprintln(w, "Error in request body")
		return
	}

	utils.Debugf(r, "Posted TransData Info: %+v", &transDataIn)

	httpClient := utils.NewHttpClient(r)

	req, err := newTransRequest(r, transDataIn)
	if err != nil {
		utils.Errorf(r, "ERROR:::: %+v", err)
		w.WriteHeader(http.StatusInternalServerError)
		fmt.Fprintln(w, "Could Not Create Translate Request")
		return
	}

	// Check this out: https://gist.github.com/kendellfab/6341981
	resp, err := httpClient.Do(req)
	if err != nil {
		utils.Errorf(r, "ERROR:::: %+v", err)
		w.WriteHeader(http.StatusInternalServerError)
		fmt.Fprintln(w, "Could Not Send Translation Request")
		return
	}

	defer resp.Body.Close()

	bodyBytes, err := ioutil.ReadAll(resp.Body)
	utils.Debug(r, string(bodyBytes))
	// decode into TransData struct
	// err = json.NewDecoder(resp.Body).Decode(&transDataOut)
	if err != nil {
		utils.Errorf(r, "ERROR:::: %+v", err)
		w.WriteHeader(http.StatusInternalServerError)
		fmt.Fprintln(w, "Could Not Parse Translation Response")
		return
	}

	utils.Debug(r, "Successful Translation!")

	utils.RenderJSON(w, r, transDataOut)
}

func newTransRequest(r *http.Request, transDataIn model.TransData) (req *http.Request, err error) {
	url := utils.RenderString("translation_request_v2", transDataIn)
	utils.Infof(r, "Translation Request: %+v", url)
	req, err = http.NewRequest("GET", url, nil)
	transAPIKey := os.Getenv("TRANSLATION_API_KEY")
	req.Header.Set("Accept", "application/json")
	req.Header.Set("Authorization", "Bearer "+transAPIKey)
	utils.Debugf(r, "Request Headers: %+v", req.Header)
	return
}
