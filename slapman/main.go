package main

import (
	"fmt"
	"os"
	"runtime"
	"slapman/cmd"
)

// Variables to identiy the build
var (
	Version  string
	Revision string
)

func main() {
	fmt.Printf("Version: %+v\n", Version)
	fmt.Printf("Git commit hash: %+v\n", Revision)
	fmt.Printf("OS: %s -- Architecture: %s\n", runtime.GOOS, runtime.GOARCH)

	mode := os.Getenv("MODE")
	if len(mode) == 0 {
		mode = "prod"
	} else {
		mode = "dev"
	}
	os.Setenv("MODE", mode)
	fmt.Printf("Mode: %+v\n", mode)

	// Adjust Go Routines
	//	runtime.GOMAXPROCS(runtime.NumCPU())
	runtime.GOMAXPROCS(runtime.NumCPU() * 5)

	cmd.Execute()
}
