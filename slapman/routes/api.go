package routes

import (
	"slapman/services"
	"slapman/utils"

	"github.com/gorilla/mux"
)

func NewServicesRouter() *mux.Router {
	router := mux.NewRouter().PathPrefix("/api").Subrouter().StrictSlash(true)

	servicesRoutes := Routes{
		Route{
			"Login",
			"POST",
			"/auth/login",
			make([]string, 0),
			services.LoginHandler,
		},
		Route{
			"Register",
			"POST",
			"/auth/register",
			make([]string, 0),
			services.RegisterHandler,
		},
		Route{
			"Translate",
			"POST",
			"/translate",
			make([]string, 0),
			services.Translate,
		},
		Route{
			"Fetch",
			"GET",
			"/tasks/fetch/{url}",
			make([]string, 0),
			services.Fetch,
		},
	}

	for _, route := range servicesRoutes {
		if len(route.Queries) == 0 {
			router.HandleFunc(route.Pattern, route.HandlerFunc).Methods(route.Method).Name(route.Name)
		} else {
			router.HandleFunc(route.Pattern, route.HandlerFunc).Queries(route.Queries...).Methods(route.Method).Name(route.Name)
		}
	}
	utils.Infof(nil, "Mux Entries %+v", router)
	return router
}
