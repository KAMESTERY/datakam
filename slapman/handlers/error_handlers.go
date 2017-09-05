package handlers

import (
	"slapman/utils"
	"net/http"
)

func NotFound(w http.ResponseWriter, r *http.Request) {
	errorData := struct {
		Code    int
		Color   string // 403 => #ff485e, 404 => #ff7709, 500s => #DF5643
		Message string
	}{
		http.StatusNotFound,
		"#ff7709",
		http.StatusText(http.StatusNotFound),
	}
	utils.RenderTemplate(w, r, "error.html", errorData)
}
