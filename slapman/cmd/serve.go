// Copyright © 2017 NAME HERE <EMAIL ADDRESS>
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package cmd

import (
	"fmt"
	"net/http"
	"net/http/fcgi"
	"os"

	"github.com/spf13/cobra"

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

	// serveCmd represents the serve command
	serveCmd = &cobra.Command{
		Use:   "serve",
		Short: "A brief description of your command",
		Long: `A longer description that spans multiple lines and likely contains examples
and usage of using your command. For example:

Cobra is a CLI library for Go that empowers applications.
This application is a tool to generate the needed files
to quickly create a Cobra application.`,
		Run: func(cmd *cobra.Command, args []string) {
			fmt.Println("serve called")
			serve()
		},
	}
)

// App Init
func init() {
	RootCmd.AddCommand(serveCmd)

	// Here you will define your flags and configuration settings.

	// Cobra supports Persistent Flags which will work for this command
	// and all subcommands, e.g.:
	// serveCmd.PersistentFlags().String("foo", "", "A help for foo")

	// Cobra supports local flags which will only run when this command
	// is called directly, e.g.:
	// serveCmd.Flags().BoolP("toggle", "t", false, "Help message for toggle")

	http.Handle("/", routes.SetupWeb())
	http.Handle("/api/", routes.SetupAPI())
	http.Handle("/echo/", sockjs.NewHandler("/echo", sockjs.DefaultOptions, handlers.SockEchoHandler))

	// static content
	//	static_content := http.StripPrefix("/static/",
	//		http.FileServer(http.Dir("./static")))
	//	http.Handle("/static/", static_content)

	//	http.Handle("/", routes)
}

// Serve: Serve the Application on HTTP or FCGI
func serve() {
	errs := make(chan error)
	go func() {
		c := make(chan os.Signal)
		signal.Notify(c, utils.SIGINT, utils.SIGTERM)
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
