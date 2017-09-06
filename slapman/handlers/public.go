package handlers

import (
	"net/http"
	"slapman/utils"
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
