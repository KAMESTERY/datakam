package handlers

import (
	"fmt"
	"github.com/gorilla/mux"
	//	"slapman/sql"
	"slapman/utils"
	"net/http"
	"os"
	"runtime"
)

func Hellofcgi(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "Hello from Fast CGI's: (CPUS:%+v, FCGI:%+v, DB_DRIVER:%+v)",
		runtime.NumCPU(), os.Getenv("FCGI"), os.Getenv("DB_DRIVER"))
}

func DbSelect(w http.ResponseWriter, r *http.Request) {
	//	db := sql.NewDBConn()
	//	defer db.Close()
	//	roles := []model.Role{}
	//	// Execute the query
	//	err := db.Select(&roles, "select * from roles")
	//	if err != nil {
	//		aego_log.Error("ERROR::::", err)
	//	}
	//	for _, role := range roles {
	//		aego_log.Debugf("Role: %+v", role)
	//		aego_log.Debugf("id: %+v", role.ID)
	//		aego_log.Debugf("name: %+v", role.Name)
	//		aego_log.Debugf("default: %+v", role.Default)
	//		aego_log.Debugf("permissions: %+v", role.Permissions)
	//		aego_log.Debugf("-----------------------------------")
	//	}
	//
	//	// Loop through rows using only one struct
	//	aego_log.Debugf("--------------- db.Queryx --------------------")
	//	role := model.Role{}
	//	rows, err := db.Queryx("select * from roles")
	//	for rows.Next() {
	//		err := rows.StructScan(&role)
	//		if err != nil {
	//			aego_log.Error("ERROR::::", err)
	//		}
	//		aego_log.Debugf("Role: %+v", role)
	//		aego_log.Debugf("id: %+v", role.ID)
	//		aego_log.Debugf("name: %+v", role.Name)
	//		aego_log.Debugf("default: %+v", role.Default)
	//		aego_log.Debugf("permissions: %+v", role.Permissions)
	//		aego_log.Debugf("-----------------------------------")
	//	}

	p := struct {
		Title string
		Body  string
		Tmpl  map[string]string
	}{
		"Welcome",
		"Some data has been retrieved from the Database",
		map[string]string{
			"NavTmpl":     "navigation",
			"ContentTmpl": "other_content",
		},
	}

	//	p := &model.Page{Title: "Welcome", Body: []byte("This is the landing model.Page")}
	utils.RenderTemplate(w, r, "base_material.html", p)
}

func SampleMD(w http.ResponseWriter, r *http.Request) {
	utils.Debugf(r, "Vars: %v", mux.Vars(r))
	pageName := mux.Vars(r)["pageName"]
	mdPage, err := utils.LoadMarkdown(pageName)
	if err != nil {
		panic(err.Error())
	}
	utils.RenderTemplate(w, r, "base_material.html", mdPage)
}

func Handler(w http.ResponseWriter, r *http.Request) {
	utils.Debug(r, "Just saying hello AE...")

	p := struct {
		Title string
		Body  string
		Tmpl  map[string]string
	}{
		"Hello",
		"Hello, world AE_GO!",
		map[string]string{
			"NavTmpl":     "navigation",
			"ContentTmpl": "other_content",
		},
	}

	utils.RenderTemplate(w, r, "base_material.html", p)
	//	fmt.Fprint(w, "Hello, world AE_GO!")
}

func SweetHandler(w http.ResponseWriter, r *http.Request) {
	utils.Debug(r, "Very sweet!!!!")
	//	go aego_log.Printf("Very sweet!!!!")

	p := struct {
		Title string
		Body  string
		Tmpl  map[string]string
	}{
		"Sweetie",
		"She is Huggable indeed!!!!",
		map[string]string{
			"NavTmpl":     "navigation",
			"ContentTmpl": "other_content",
		},
	}

	utils.RenderTemplate(w, r, "base_material.html", p)
	//	fmt.Fprint(w, "She is Huggable indeed!!!!")
}

func EmailHandler(w http.ResponseWriter, r *http.Request) {
	success, err := utils.SendEmail(r)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	p := struct {
		Title string
		Body  string
		Tmpl  map[string]string
	}{
		"Email Request",
		success,
		map[string]string{
			"NavTmpl":     "navigation",
			"ContentTmpl": "detail",
		},
	}

	utils.RenderTemplate(w, r, "base_material.html", p)
}
