package utils

import (
	"crypto/rand"
	"crypto/sha512"
	"encoding/hex"
	"errors"
	"fmt"
	"strconv"
	"strings"
	"unicode"

	"golang.org/x/crypto/bcrypt"
	"golang.org/x/crypto/pbkdf2"

	"github.com/segmentio/ksuid"
)

const (
	alphanum    = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
	longueurSel = 20
)

// GetRandomString Random generate string
func GetRandomString(n int) string {
	byteArr := make([]byte, n)
	rand.Read(byteArr)
	for i, b := range byteArr {
		byteArr[i] = alphanum[b%byte(len(alphanum))]
	}
	randStr := string(byteArr)
	return randStr
}

// EncodeStr Encode string
func EncodeStr(rawStr string, saltStr string) string {
	encodedBytes := pbkdf2.Key([]byte(rawStr), []byte(saltStr), 10000, 50, sha512.New)
	encodedStr := hex.EncodeToString(encodedBytes)
	return encodedStr
}

// EncodeString Encode string
func EncodeString(str string) string {
	salt := GetRandomString(40)
	encodedPwd := salt + "$" + EncodeStr(str, salt)
	return encodedPwd
}

// VerifyPassword Verify string
func VerifyPassword(rawPwd string, encodedPwd string) bool {
	// split
	var salt, encoded string
	salt = encodedPwd[:longueurSel]
	encoded = encodedPwd[longueurSel+1:]
	return EncodeStr(rawPwd, salt) == encoded
}

// HashPassword Hash password
func HashPassword(password []byte) ([]byte, error) {
	return bcrypt.GenerateFromPassword(password, 14)
}

// CheckPasswordHash Check Password
func CheckPasswordHash(hash, password []byte) error {
	return bcrypt.CompareHashAndPassword(hash, password)
}

// GenerateSlug Borrowed from here: https://gowalker.org/github.com/mrvdot/golang-utils
func GenerateSlug(str string) string {

	return strings.Map(func(r rune) rune {
		switch {
		case r == ' ', r == '-':
			return '-'
		case r == '_', unicode.IsLetter(r), unicode.IsDigit(r):
			return r
		default:
			return -1
		}
	}, strings.ToLower(strings.TrimSpace(str)))
}

func ParseInt64(d interface{}) (i64 int64, err error) {
	i64str, ok := d.(string)
	if !ok {
		errMsg := fmt.Sprintf("Input is not a valid String: +%v", d)
		err = errors.New(errMsg)
		return
	}
	i64, err = strconv.ParseInt(i64str, 10, 64)
	return
}

func GenerateUUID() string {
	uuid := ksuid.New()
	return uuid.String()
}
