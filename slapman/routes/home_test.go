package routes

import (
	"reflect"
	"testing"

	"github.com/gorilla/mux"
)

func TestPublicRoutes(t *testing.T) {
	tests := []struct {
		name string
		want *mux.Router
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		if got := PublicRoutes(); !reflect.DeepEqual(got, tt.want) {
			t.Errorf("%q. PublicRoutes() = %v, want %v", tt.name, got, tt.want)
		}
	}
}
