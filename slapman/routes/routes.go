package routes

import (
	"slapman/middleware"
	"slapman/utils/alice"
	"net/http"

	"github.com/gorilla/context"
	"github.com/gorilla/handlers"
	"github.com/gorilla/mux"
	"github.com/justinas/nosurf"
)

// Route Type for Route
type Route struct {
	Name        string
	Method      string
	Pattern     string
	Queries     []string
	HandlerFunc http.HandlerFunc
}

// Routes List of Routes Type
type Routes []Route

// SetupWeb Sets up Web Routes
func SetupWeb() http.Handler {
	baseRoutes := PublicRoutes()
	//baseRoutes.NotFoundHandler = http.HandlerFunc(slapmanhandlers.NotFound) //TODO: Is this needed?

	publicRoutes := wrapCommonMiddlewares(
		baseRoutes,
		nosurf.NewPure, // CSRF Protection
	)

	return publicRoutes
}

// SetupAPI Sets up API Routes
func SetupAPI() http.Handler {
	baseRoutes := NewServicesRouter()
	//baseRoutes.NotFoundHandler = http.HandlerFunc(slapmanhandlers.NotFound) //TODO: Figure out something for JSON 404

	servicesRoutes := wrapCommonMiddlewares(
		baseRoutes,
		middleware.JWTMiddleware,
	)

	return servicesRoutes
}

func wrapCommonMiddlewares(routes *mux.Router, extras ...alice.Constructor) http.Handler {
	middleWares := alice.New(
		context.ClearHandler,
		handlers.HTTPMethodOverrideHandler,
		middleware.CustomHeaders,
		middleware.ClientIP,
		middleware.RequestCaching,
		//middleware.RequestLogger(os.Stdout),
		// middleware.WithDB,
		middleware.RecoveryNew,
		handlers.CompressHandler,
	).Append(extras...)

	return middleWares.Then(routes)
}
