package middleware

import (
	"fmt"
	"net/http"
	"slapman/utils"
	"strings"

	jwt "github.com/dgrijalva/jwt-go"
	jwtReq "github.com/dgrijalva/jwt-go/request"
)

var jwt_logger = utils.NewLogger("middlewarejwt")

func pathContains(req *http.Request, chunks ...string) (match bool) {
	for _, chunk := range chunks {
		if match = strings.Contains(req.URL.String(), chunk); match {
			jwt_logger.Debug("Can Proceed without JWT")
			break
		}
	}
	jwt_logger.Debug("Cannot Proceed without JWT")
	return
}

// JWTMiddleware JWT Middleware
func JWTMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(rw http.ResponseWriter, req *http.Request) {

		if !pathContains(req, "login", "register") {
			// Validate the JWT Token
			jwt_logger.Debug("Validating JWT Token...")
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
				jwt_logger.Debugf("Valid Token: %+v", token)
				next.ServeHTTP(rw, req)
			} else {
				jwt_logger.Errorf("Unauthorized Request: %+v", err)
				rw.WriteHeader(http.StatusUnauthorized)
				fmt.Fprint(rw, "Authentication failed")
			}
		} else {
			next.ServeHTTP(rw, req)
		}
	})
}
