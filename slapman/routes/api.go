package routes

import (
	"slapman/services"

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
			"Fetch",
			"GET",
			"/tasks/fetch/{url}",
			make([]string, 0),
			services.Fetch,
		},
		Route{
			"GqlOPTIONS",
			"OPTIONS",
			"/graphql",
			make([]string, 0),
			services.HandleGqlRequest,
		},
		Route{
			"GqlGET",
			"GET",
			"/graphql",
			make([]string, 0),
			services.HandleGqlRequest,
		},
		Route{
			"GqlPOST",
			"POST",
			"/graphql",
			make([]string, 0),
			services.HandleGqlRequest,
		},
	}

	for _, route := range servicesRoutes {
		if len(route.Queries) == 0 {
			router.HandleFunc(route.Pattern, route.HandlerFunc).Methods(route.Method).Name(route.Name)
		} else {
			router.HandleFunc(route.Pattern, route.HandlerFunc).Queries(route.Queries...).Methods(route.Method).Name(route.Name)
		}
	}
	return router
}
