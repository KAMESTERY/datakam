package middleware

import (
	"slapman/utils"
	"net/http"
)

// ClientIP Adds client IP to Context
func ClientIP(h http.Handler) http.Handler {
	return http.HandlerFunc(func(rw http.ResponseWriter, req *http.Request) {

		// set client ip address
		utils.SetClientIP(req)

		h.ServeHTTP(rw, req)
	})
}
