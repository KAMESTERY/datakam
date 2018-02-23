package utils

import (
	"os"
	"path/filepath"

	"github.com/gobuffalo/packr"
)

var (
	baseDir = func() string {
		bd := os.Getenv("BASEDIR")
		if len(bd) == 0 {
			dir, err := filepath.Abs(filepath.Dir(os.Args[0]))
			if err != nil {
				dir = "."
			}
			bd = dir
		}
		return bd
	}()

	//TemplatesDir = baseDir + "/templates/*"
	TemplatesDir = baseDir + "/../templates"
	MarkdownsDir = baseDir + "/../pages/"
	StaticDir    = baseDir + "/../static"

	// Boxes
	TemplatesBox = packr.NewBox("../templates")
	MarkdownsBox = packr.NewBox("../pages/")
	StaticBox    = packr.NewBox("../static")
	KeysBox      = packr.NewBox("../keys")
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
