package utils

import (
	"html/template"
	"net/http"
)

// GetParam Get Parameter from Request
func GetParam(r *http.Request, paramName string) string {
	param := template.HTMLEscapeString(r.Form.Get(paramName))
	return param
}
