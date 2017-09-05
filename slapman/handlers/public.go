package handlers

import (
	"fmt"
	"slapman/utils"
	"net/http"
)

// Home serves home page
func Home(w http.ResponseWriter, r *http.Request) {
	defer w.WriteHeader(http.StatusOK)
	utils.Debug(r, "Serving Home Page")
	fmt.Fprint(w, "Welcome Home!!!!")
}
