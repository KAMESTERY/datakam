package handlers

import (
	"slapman/utils"
	"net/http"
)

// Home serves home page
func Home(w http.ResponseWriter, r *http.Request) {
	utils.Debug(r, "Serving Home Page")
	m := struct {
		Msg string
	}{
		"Welcome Home!!!!",
	}
	utils.RenderJSON(w, r, m)
}
