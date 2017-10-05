package main

import (
	"fmt"
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

	// Adjust Go Routines
	//	runtime.GOMAXPROCS(runtime.NumCPU())
	runtime.GOMAXPROCS(runtime.NumCPU() * 5)

	cmd.Execute()
}
