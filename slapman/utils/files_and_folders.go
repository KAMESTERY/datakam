package utils

import "os"

var (
	baseDir = func() string {
		bd := os.Getenv("BASEDIR")
		if len(bd) == 0 {
			bd = "."
		}
		return bd
	}()

	// using asymmetric crypto/RSA keys
	// location of the files used for signing and verification
	privKeyPath = baseDir + "/keys/slapman.rsa"     // openssl genrsa -out app.rsa 1024
	pubKeyPath  = baseDir + "/keys/slapman.rsa.pub" // openssl rsa -in app.rsa -pubout > app.rsa.pub

	//TemplatesDir = baseDir + "/templates/*"
	TemplatesDir = baseDir + "/templates"
	MarkdownsDir = baseDir + "/pages/"
	StaticDir    = baseDir + "/static"
)

// exists returns whether the given file or directory exists or not
func pathExists(path string) (bool, error) {
	_, err := os.Stat(path)
	if err == nil {
		return true, nil
	}
	if os.IsNotExist(err) {
		return false, nil
	}
	return true, err
}
