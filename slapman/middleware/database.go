package middleware

import (
	"net/http"
	"slapman/model"
)

// WithDB adds Database Connection to Context
func WithDB(h http.Handler) http.Handler {
	return http.HandlerFunc(func(rw http.ResponseWriter, req *http.Request) {
		dbConn := model.NewDBConn(req)
		model.SetDbConn(req, dbConn)

		h.ServeHTTP(rw, req)
	})
}
