package utils

import (
	"crypto/rsa"
	"io/ioutil"

	jwt "github.com/dgrijalva/jwt-go"
)

const (
	DbCtxKey     = "db"
	DaoCtxKey    = "dao"
	UserIpCtxKey = "userIP"
)

// verify key and sign key
var (
	SignKey   *rsa.PrivateKey
	VerifyKey *rsa.PublicKey
)

// read the key files before starting http handlers
func init() {
	var err error

	signBytes, err := ioutil.ReadFile(privKeyPath)
	if err != nil {
		Fatal(nil, "Error reading private key %+v", err)
		return
	}
	SignKey, err = jwt.ParseRSAPrivateKeyFromPEM(signBytes)
	if err != nil {
		Fatal(nil, "Error parsing private key bytes %+v", err)
		return
	}

	verifyBytes, err := ioutil.ReadFile(pubKeyPath)
	if err != nil {
		Fatal(nil, "Error reading public key %+v", err)
		return
	}
	VerifyKey, err = jwt.ParseRSAPublicKeyFromPEM(verifyBytes)
	if err != nil {
		Fatal(nil, "Error parsing public key bytes %+v", err)
		return
	}
}
