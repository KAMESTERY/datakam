package utils

import (
	"github.com/gorilla/securecookie"
)

//var (
//	hashKey  []byte
//	blockKey []byte
//	sc       *securecookie.SecureCookie
//)

func Cookie() *securecookie.SecureCookie {
	//	hashKey = []byte("very-secret")
	//	blockKey = []byte("a-lot-secret")
	hashKey := []byte("/RO0Fl.DD'G?{9}U6Wb[A^-G4>9]PL")
	blockKey := []byte("JPHkAfm*hg+992kp|c-5R6p&?9G%lI")
	sc := securecookie.New(hashKey, blockKey)
	return sc
}
