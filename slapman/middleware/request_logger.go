package middleware

import (
	"io"
	"net/http"

	"github.com/gorilla/handlers"
)

// RequestLogger Logs all Requests
func RequestLogger(out io.Writer) func(http.Handler) http.Handler {
	return func(next http.Handler) http.Handler {
		return handlers.CombinedLoggingHandler(out, next)
	}
}
