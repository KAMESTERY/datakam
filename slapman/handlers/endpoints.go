package handlers

import (
	"fmt"
	//	"slapman/sql"

	"net/http"
	"os"
	"runtime"
	"slapman/utils"
)

var endpoints_logger = utils.NewLogger("handlersendpoints")

func Hellofcgi(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "Hello from Fast CGI's: (CPUS:%+v, FCGI:%+v, DB_DRIVER:%+v)",
		runtime.NumCPU(), os.Getenv("FCGI"), os.Getenv("DB_DRIVER"))
}

func Handler(w http.ResponseWriter, r *http.Request) {
	endpoints_logger.Debug("Just saying hello AE...")

	p := struct {
		Title string
		Body  string
	}{
		"Hello",
		"Hello, world AE_GO!",
	}

	utils.RenderJSON(w, r, p)
	//	fmt.Fprint(w, "Hello, world AE_GO!")
}

func SweetHandler(w http.ResponseWriter, r *http.Request) {
	endpoints_logger.Debug("Very sweet!!!!")
	//	go aego_log.Printf("Very sweet!!!!")

	p := struct {
		Title string
		Body  string
	}{
		"Sweetie",
		"She is Huggable indeed!!!!",
	}

	utils.RenderJSON(w, r, p)
	//	fmt.Fprint(w, "She is Huggable indeed!!!!")
}

func EmailHandler(w http.ResponseWriter, r *http.Request) {
	success, err := utils.SendEmail(r.Context())
	if err != nil {
		//http.Error(w, err.Error(), http.StatusInternalServerError)
		utils.RenderJSONWithCode(w, r, err.Error(), http.StatusInternalServerError)
		return
	}

	p := struct {
		Title string
		Body  string
	}{
		"Email Request",
		success,
	}

	utils.RenderJSON(w, r, p)
}
