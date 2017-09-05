package middleware

import (
	"net/http"

	"github.com/lox/httpcache"
	"github.com/lox/httpcache/httplog"
)

// RequestCaching Caches Response
func RequestCaching(next http.Handler) http.Handler {
	return http.HandlerFunc(func(rw http.ResponseWriter, req *http.Request) {

		cachingHandler := httpcache.NewHandler(httpcache.NewMemoryCache(), next)
		cachingHandler.Shared = true

		handler := httplog.NewResponseLogger(cachingHandler)

		//utils.Debug(req, "Through Caching Middleware...")
		httpcache.DebugLogging = true

		// Call the Caching middleware handler
		handler.ServeHTTP(rw, req)
	})
}
