package main

import (
	"github.com/astaxie/beego"
	"os"
	_ "slapman.com/routers"
	"strconv"
)

func main() {
	if fcgi_enabled := os.Getenv("FCGI"); len(fcgi_enabled) == 0 {
		port, err := strconv.Atoi(os.Getenv("PORT"))
		if err != nil {
			port = 8899
		}
		beego.BConfig.Listen.HTTPPort = port
	} else {
		beego.BConfig.Listen.EnableFcgi = true
		beego.BConfig.Listen.EnableStdIo = true
	}
	beego.Run()
}
