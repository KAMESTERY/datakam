package middleware

import (
	"fmt"
	"net/http"
	"slapman/utils"
	"strings"

	jwt "github.com/dgrijalva/jwt-go"
	jwtReq "github.com/dgrijalva/jwt-go/request"
)

func pathContains(req *http.Request, chunks ...string) (match bool) {
	for _, chunk := range chunks {
		if match = strings.Contains(req.URL.String(), chunk); match {
			utils.Debug(req, "Can Proceed without JWT")
			break
		}
	}
	utils.Debug(req, "Cannot Proceed without JWT")
	return
}

// JWTMiddleware JWT Middleware
func JWTMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(rw http.ResponseWriter, req *http.Request) {

		if !pathContains(req, "login", "register") {
			// Validate the JWT Token
			utils.Debug(req, "Validating JWT Token...")
			extractor := jwtReq.MultiExtractor{
				// jwtReq.HeaderExtractor{"Token"},
				// jwtReq.ArgumentExtractor{"token"},
				jwtReq.AuthorizationHeaderExtractor,
			}
			token, err := jwtReq.ParseFromRequestWithClaims(req, extractor, &jwt.StandardClaims{}, func(token *jwt.Token) (interface{}, error) {
				return utils.VerifyKey, nil
			})
			if err == nil && token.Valid {
				// Call the next middleware handler
				utils.Debugf(req, "Valid Token: %+v", token)
				next.ServeHTTP(rw, req)
			} else {
				utils.Errorf(req, "Unauthorized Request: %+v", err)
				rw.WriteHeader(http.StatusUnauthorized)
				fmt.Fprint(rw, "Authentication failed")
			}
		} else {
			next.ServeHTTP(rw, req)
		}
	})
}
