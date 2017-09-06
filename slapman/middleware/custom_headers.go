package middleware

import (
	"crypto/md5"
	"encoding/hex"
	"fmt"
	"io"
	"net/http"
	"slapman/utils"
	"strings"
	"time"
)

var (
	cacheSince = time.Now().Format(http.TimeFormat)
	cacheUntil = time.Now().AddDate(60, 0, 0).Format(http.TimeFormat)
)

// CustomHeaders Sets Common Custom Headers
func CustomHeaders(next http.Handler) http.Handler {
	return http.HandlerFunc(func(rw http.ResponseWriter, req *http.Request) {

		// Set Custom Headers
		rw.Header().Set("X-Frame-Options", "deny")
		rw.Header().Set("X-XSS-Protection", "1; mode=block")
		rw.Header().Set("X-Content-Type-Options", "nosniff")
		rw.Header().Set("X-Powered-By", "Outcastgeek Polyglot System")
		rw.Header().Set("X-With-Special-Sauce", "Static Habanero")

		// Cache Headers
		rw.Header().Set("Vary", "Accept-Encoding")
		rw.Header().Set("Cache-Control", "public, max-age=3600") // 1 hour
		rw.Header().Set("Last-Modified", cacheSince)
		rw.Header().Set("Expires", cacheUntil)

		// Etag Header
		etagHasher := md5.New()
		keyBase := fmt.Sprintf("slapman-%s-%s", req.Method, req.URL.String())
		utils.Debugf(req, "Path: %s", keyBase)
		io.WriteString(etagHasher, keyBase)
		key := hex.EncodeToString(etagHasher.Sum(nil))
		e := `"` + key + `"`
		rw.Header().Set("Etag", e)
		if match := req.Header.Get("If-None-Match"); match != "" {
			if strings.Contains(match, e) {
				rw.WriteHeader(http.StatusNotModified)
				return
			}
		}

		// Call the next middleware handler
		next.ServeHTTP(rw, req)
	})
}
