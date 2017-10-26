package utils

import (
	"fmt"

	"github.com/dgrijalva/jwt-go"
)

type JwtToken struct {
	Token string `json:"token"`
}

var jwtutils_logger = NewLogger("utilsjwt")

func GenerateRsa256JwtToken(claims jwt.Claims) (token JwtToken, err error) {

	jwtutils_logger.Debug("Generating JWT Token...")

	t := jwt.NewWithClaims(jwt.SigningMethodRS256, claims)
	tokenString, err := t.SignedString(SignKey)
	if err != nil {
		jwtutils_logger.Errorf("ERROR:::: Token Signing error: %+v\n", err)
		err = fmt.Errorf("Sorry, error while Signing Token!")
		return
	}

	token.Token = tokenString

	jwtutils_logger.Debugf("JWT Token has been Generated")

	return
}

func ValidateRsa256JwtTokenInParams(params map[string]interface{}) (err error) {
	tokenString := params["token"].(string)
	_, err = ValidateRsa256JwtToken(tokenString)
	return
}

func ValidateRsa256JwtToken(tokenString string) (token *jwt.Token, err error) {

	jwtutils_logger.Debugf("Validating JWT Token...")

	token, err = jwt.Parse(tokenString, func(token *jwt.Token) (interface{}, error) {
		return VerifyKey, nil
	})

	if err != nil || !token.Valid || token.Header["alg"] != jwt.SigningMethodRS256.Alg() {
		jwtutils_logger.Errorf("ERROR:::: JWT Token is Invalid: %+v", err)
		return
	}

	jwtutils_logger.Debugf("JWT Token is Valid")

	return
}

func RefreshRsa256JwtToken(tokenString string) (newToken JwtToken, claims jwt.Claims, err error) {

	jwtutils_logger.Debugf("Refreshing JWT Token...")

	token, err := ValidateRsa256JwtToken(tokenString)

	if valid := err == nil && token.Valid && token.Method == jwt.SigningMethodRS256; !valid {
		jwtutils_logger.Errorf("ERROR::: %+v", err)
		err = fmt.Errorf("Cannot Refresh an Invalid Token")
		return
	}

	claims = token.Claims
	newToken, err = GenerateRsa256JwtToken(claims)

	jwtutils_logger.Debugf("JWT Token has been Refreshed")

	return
}
