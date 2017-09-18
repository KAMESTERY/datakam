package main

import (
	"fmt"
	"net/http"
	"net/http/fcgi"
	"os"
	"runtime"
	"slapman/cmd"
	"slapman/handlers"
	"slapman/routes"
	"slapman/utils"

	"os/signal"

	"gopkg.in/igm/sockjs-go.v2/sockjs"

	_ "github.com/go-sql-driver/mysql"
)

// Variables to identiy the build
var (
	Version  string
	Revision string
)

// App Init
func init() {
	utils.Infof(nil, "Version: %+v", Version)
	utils.Infof(nil, "Git commit hash: %+v", Revision)
	utils.Infof(nil, "OS: %s -- Architecture: %s", runtime.GOOS, runtime.GOARCH)

	mode := os.Getenv("MODE")
	if len(mode) == 0 {
		mode = "prod"
	} else {
		mode = "dev"
	}
	os.Setenv("MODE", mode)
	utils.Infof(nil, "Mode: %+v", mode)

	// Adjust Go Routines
	//	runtime.GOMAXPROCS(runtime.NumCPU())
	runtime.GOMAXPROCS(runtime.NumCPU() * 5)

	http.Handle("/", routes.SetupWeb())
	http.Handle("/api/", routes.SetupAPI())
	http.Handle("/echo/", sockjs.NewHandler("/echo", sockjs.DefaultOptions, handlers.SockEchoHandler))

	// static content
	//	static_content := http.StripPrefix("/static/",
	//		http.FileServer(http.Dir("./static")))
	//	http.Handle("/static/", static_content)

	//	http.Handle("/", routes)
}

func main() {
	errs := make(chan error)
	go func() {
		c := make(chan os.Signal)
		signal.Notify(c, cmd.SIGINT, cmd.SIGTERM)
		errs <- fmt.Errorf("%s", <-c)
	}()

	go func() {
		if fcgi_enabled := os.Getenv("FCGI"); len(fcgi_enabled) == 0 {
			port := os.Getenv("PORT")
			if len(port) == 0 {
				port = "1112"
			}
			host := os.Getenv("HOST")
			utils.Infof(nil, "Routes created, now serving on  %+v: %+v", host, port)
			errs <- http.ListenAndServe(host+":"+port, nil)
		} else {
			utils.Infof(nil, "Serving on FCGI")
			errs <- fcgi.Serve(nil, nil)
			//		errs <- fcgi.Serve(nil, )
		}
	}()

	// Wait
	utils.Errorf(nil, "%+v", <-errs)
}

// Got it from here: https://gist.github.com/hyg/9c4afcd91fe24316cbf0
//func OpenBrowser(url string) {
//	var err error
//
//	switch runtime.GOOS {
//	case "linux":
//		err = exec.Command("xdg-open", url).Start()
//	case "windows":
//		err = exec.Command("rundll32", "url.dll,FileProtocolHandler", url).Start()
//	case "darwin":
//		err = exec.Command("open", url).Start()
//	default:
//		err = fmt.Errorf("unsupported platform")
//	}
//	if err != nil {
//		log.Fatal(err)
//	}
//
//}