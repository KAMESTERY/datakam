package handlers

import (
	"fmt"
	"net/http"
	"slapman/utils"
	"strings"

	"github.com/gorilla/mux"
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

// Admin serves admin page
func Admin(w http.ResponseWriter, r *http.Request) {
	public_logger.Debug("Serving Admin Page")
	w.Header().Set("Content-Type", "text/html; charset=UTF-8")
	html, err := utils.TemplatesBox.MustString("graphql.html")
	if err != nil {
		public_logger.Warnf("WARNING:::: %+v", err)
	}
	fmt.Fprint(w, html)
}

// Static serves static assets
func Static(w http.ResponseWriter, r *http.Request) {
	public_logger.Debug("Serving Static Files")

	vars := mux.Vars(r)
	category := vars["category"]
	filePath := vars["filePath"]

	fullFilePath := strings.Join([]string{category, filePath}, "/")

	public_logger.Debugf("Serving File: %s", fullFilePath)

	if strings.Contains(fullFilePath, ".css") {
		w.Header().Set("Content-Type", "text/css; charset=UTF-8")
	}

	if strings.Contains(fullFilePath, ".js") {
		w.Header().Set("Content-Type", "application/javascript; charset=UTF-8")
	}

	file, err := utils.StaticBox.MustString(fullFilePath)
	if err != nil {
		public_logger.Warnf("WARNING:::: %+v", err)
	}
	fmt.Fprint(w, file)
}
