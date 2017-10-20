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
	SignKey     *rsa.PrivateKey
	VerifyKey   *rsa.PublicKey
	signBytes   []byte
	verifyBytes []byte
	err         error
	keys_logger = NewLogger("utilskeys")
)

// read the key files before starting http handlers
func SetupCrypto(cryptoRsa string) {
	// using asymmetric crypto/RSA keys
	// location of the files used for signing and verification
	privKeyPath := baseDir + "/" + cryptoRsa + ".rsa"    // openssl genrsa -out app.rsa 1024
	pubKeyPath := baseDir + "/" + cryptoRsa + ".rsa.pub" // openssl rsa -in app.rsa -pubout > app.rsa.pub

	signBytes, err = ioutil.ReadFile(privKeyPath)
	if err != nil {
		keys_logger.Fatalf("Error reading private key %+v", err)
		return
	}
	SignKey, err = jwt.ParseRSAPrivateKeyFromPEM(signBytes)
	if err != nil {
		keys_logger.Fatalf("Error parsing private key bytes %+v", err)
		return
	}

	verifyBytes, err = ioutil.ReadFile(pubKeyPath)
	if err != nil {
		keys_logger.Fatalf("Error reading public key %+v", err)
		return
	}
	VerifyKey, err = jwt.ParseRSAPublicKeyFromPEM(verifyBytes)
	if err != nil {
		keys_logger.Fatalf("Error parsing public key bytes %+v", err)
		return
	}
}
