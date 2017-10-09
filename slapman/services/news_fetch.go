package services

import (
	"net/http"
	"slapman/utils"

	"github.com/gorilla/mux"
)

var news_logger = utils.NewLogger("servicesnews")

// Fetch retrieves the url content contain and renders its body into a template
func Fetch(w http.ResponseWriter, r *http.Request) {

	url := mux.Vars(r)["url"]

	resp, err := utils.GetString(r.Context(), "http://"+url)
	if err != nil {
		errorData := struct {
			Code    int
			Color   string // 403 => #ff485e, 404 => #ff7709, 500s => #DF5643
			Message string
		}{
			http.StatusBadRequest,
			"#ff485e",
			http.StatusText(http.StatusBadRequest),
		}
		utils.RenderJSONWithCode(w, r, errorData, http.StatusBadRequest)
		return
	}

	news_logger.Debugf("Fetched URL: %s", url)

	utils.RenderJSON(w, r, resp)
}
