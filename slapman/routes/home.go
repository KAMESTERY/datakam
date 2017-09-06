package routes

import (
	"slapman/handlers"
	"slapman/utils"
	"net/http"

	"github.com/gorilla/mux"
)

// routes
func PublicRoutes() *mux.Router {
	router := mux.NewRouter()

	// static content
	staticContent := http.StripPrefix("/static/",
		http.FileServer(http.Dir(utils.StaticDir)))
	router.PathPrefix("/static/").Handler(staticContent)

	homeRoutes := Routes{
		Route{
			"Home",
			"GET",
			"/",
			make([]string, 0),
			handlers.Home,
		},
		Route{
			"HelloFCGI",
			"GET",
			"/hellofcgi",
			make([]string, 0),
			handlers.Hellofcgi,
		},
		Route{
			"Hello",
			"GET",
			"/hello",
			make([]string, 0),
			handlers.Handler,
		},
		Route{
			"Sweet",
			"GET",
			"/sweet",
			make([]string, 0),
			handlers.SweetHandler,
		},
		Route{
			"Email",
			"GET",
			"/sendemail",
			make([]string, 0),
			handlers.EmailHandler,
		},
	}

	for _, route := range homeRoutes {
		if len(route.Queries) == 0 {
			router.HandleFunc(route.Pattern, route.HandlerFunc).Methods(route.Method).Name(route.Name)
		} else {
			router.HandleFunc(route.Pattern, route.HandlerFunc).Queries(route.Queries...).Methods(route.Method).Name(route.Name)
		}
	}

	utils.Infof(nil, "Mux Entries %+v", router)
	return router
}
