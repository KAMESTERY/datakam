package services

import (
	"encoding/json"
	"net/http"
	"slapman/model"
	"slapman/utils"
	"time"

	jwt "github.com/dgrijalva/jwt-go"
)

type jwtAuthErrors struct {
	Status string
}

// RegisterHandler registers a new user
func RegisterHandler(w http.ResponseWriter, r *http.Request) {

	var newUser model.UserIn

	// decode into UserIn struct
	err := json.NewDecoder(r.Body).Decode(&newUser)
	if err != nil {
		utils.RenderJSONWithCode(w, r, jwtAuthErrors{"Error in request body"}, http.StatusInternalServerError)
		return
	}

	utils.Debugf(r, "Posted New User: %+v", &newUser)

	// verify new user credentials
	if !newUser.Ok() {
		utils.Errorf(r, "BAD CREDS:::: %+v", newUser)
		utils.RenderJSONWithCode(w, r, jwtAuthErrors{"Invalid User Credentials Provided"}, http.StatusConflict)
		return
	}

	// validate new user information
	if userExists := newUser.FindUser(r); userExists != nil {
		utils.Errorf(r, "ALREADY EXISTS:::: %+v", newUser)
		utils.RenderJSONWithCode(w, r, jwtAuthErrors{"User already Exists"}, http.StatusConflict)
		return
	}

	// Create New User Here
	if newUser.Save(r) {
		utils.RenderJSONWithCode(w, r, jwtAuthErrors{"New User Successfully Created"}, http.StatusCreated)
	} else {
		utils.RenderJSONWithCode(w, r, jwtAuthErrors{"Could Not Create New User"}, http.StatusInternalServerError)
	}
}

// LoginHandler reads the login credentials, checks them and creates the JWT token
func LoginHandler(w http.ResponseWriter, r *http.Request) {

	utils.Debug(r, "Logging in...")

	var userInfo model.User

	utils.Debugf(r, "Request Body: %+v", r.Body)

	// decode into User struct
	err := json.NewDecoder(r.Body).Decode(&userInfo)
	if err != nil {
		utils.Errorf(r, "ERROR:::: %+v", err)
		utils.RenderJSONWithCode(w, r, jwtAuthErrors{"Error in request body"}, http.StatusInternalServerError)
		return
	}

	if !userInfo.Ok() {
		utils.Errorf(r, "INVALID USER:::: %+v", userInfo)
		utils.RenderJSONWithCode(w, r, jwtAuthErrors{"Invalid User Info"}, http.StatusBadRequest)
		return
	}

	utils.Debugf(r, "Posted User Info: %+v", userInfo)

	// validate user credentials
	if userRef := userInfo.Verify(r); userRef != nil {
		utils.Debugf(r, "Logging in User: %+v", userRef)

		// create a signer for rsa 256 and claim c
		c := struct {
			User *model.UserRef
			jwt.StandardClaims
		}{
			userRef,
			jwt.StandardClaims{
				ExpiresAt: time.Now().Add(time.Minute * 20).Unix(),
				Issuer:    "admin",
			},
		}
		t := jwt.NewWithClaims(jwt.SigningMethodRS256, c)

		utils.Debugf(r, "Sign Key: %+v", utils.SignKey)
		tokenString, err := t.SignedString(utils.SignKey)
		if err != nil {
			utils.Errorf(r, "Token Signing error: %+v\n", err)
			utils.RenderJSONWithCode(w, r, jwtAuthErrors{"Sorry, error while Signing Token!"}, http.StatusInternalServerError)
			return
		}

		//TODO: update last user login

		response := struct {
			Token string `json:"token"`
		}{tokenString}
		utils.RenderJSON(w, r, response)
	} else {
		utils.RenderJSONWithCode(w, r, jwtAuthErrors{"Wrong info"}, http.StatusForbidden)
		return
	}
}
