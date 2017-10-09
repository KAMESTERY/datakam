package handlers

import (
	"net/http"
	"slapman/utils"
)

var public_logger = utils.NewLogger("handlerspublic")

// Home serves home page
func Home(w http.ResponseWriter, r *http.Request) {
	public_logger.Debug("Serving Home Page")
	m := struct {
		Msg string
	}{
		"Welcome Home!!!!",
	}
	utils.RenderJSON(w, r, m)
}
