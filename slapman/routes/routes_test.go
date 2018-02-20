package routes

import (
	"net/http"
	"reflect"
	"testing"

	"github.com/gorilla/mux"
)

func TestSetupApp(t *testing.T) {
	tests := []struct {
		name string
		want http.Handler
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		if got := SetupApp(); !reflect.DeepEqual(got, tt.want) {
			t.Errorf("%q. SetupApp() = %v, want %v", tt.name, got, tt.want)
		}
	}
}

func Test_wrapMiddlewares(t *testing.T) {
	type args struct {
		routes *mux.Router
	}
	tests := []struct {
		name string
		args args
		want http.Handler
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		if got := wrapMiddlewares(tt.args.routes); !reflect.DeepEqual(got, tt.want) {
			t.Errorf("%q. wrapMiddlewares() = %v, want %v", tt.name, got, tt.want)
		}
	}
}
