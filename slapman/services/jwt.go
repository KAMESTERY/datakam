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

var jwt_logger = utils.NewLogger("servicesjwt")

// RegisterHandler registers a new user
func RegisterHandler(w http.ResponseWriter, r *http.Request) {

	var newUser model.UserIn

	// decode into UserIn struct
	err := json.NewDecoder(r.Body).Decode(&newUser)
	if err != nil {
		utils.RenderJSONWithCode(w, r, jwtAuthErrors{"Error in request body"}, http.StatusInternalServerError)
		return
	}

	jwt_logger.Debugf("Posted New User: %+v", &newUser)

	// verify new user credentials
	if !newUser.Ok() {
		jwt_logger.Errorf("BAD CREDS:::: %+v", newUser)
		utils.RenderJSONWithCode(w, r, jwtAuthErrors{"Invalid User Credentials Provided"}, http.StatusConflict)
		return
	}

	// validate new user information
	if userExists := newUser.FindUser(); userExists != nil {
		jwt_logger.Errorf("ALREADY EXISTS:::: %+v", newUser)
		utils.RenderJSONWithCode(w, r, jwtAuthErrors{"User already Exists"}, http.StatusConflict)
		return
	}

	// Create New User Here
	if newUser.Save() {
		utils.RenderJSONWithCode(w, r, jwtAuthErrors{"New User Successfully Created"}, http.StatusCreated)
	} else {
		utils.RenderJSONWithCode(w, r, jwtAuthErrors{"Could Not Create New User"}, http.StatusInternalServerError)
	}
}

// LoginHandler reads the login credentials, checks them and creates the JWT token
func LoginHandler(w http.ResponseWriter, r *http.Request) {

	jwt_logger.Debug("Logging in...")

	var userInfo model.User

	jwt_logger.Debugf("Request Body: %+v", r.Body)

	// decode into User struct
	err := json.NewDecoder(r.Body).Decode(&userInfo)
	if err != nil {
		jwt_logger.Errorf("ERROR:::: %+v", err)
		utils.RenderJSONWithCode(w, r, jwtAuthErrors{"Error in request body"}, http.StatusInternalServerError)
		return
	}

	if !userInfo.Ok() {
		jwt_logger.Errorf("INVALID USER:::: %+v", userInfo)
		utils.RenderJSONWithCode(w, r, jwtAuthErrors{"Invalid User Info"}, http.StatusBadRequest)
		return
	}

	jwt_logger.Debugf("Posted User Info: %+v", userInfo)

	// validate user credentials
	if userRef := userInfo.Verify(); userRef != nil {
		jwt_logger.Debugf("Logging in User: %+v", userRef)

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

		jwt_logger.Debugf("Sign Key: %+v", utils.SignKey)
		tokenString, err := t.SignedString(utils.SignKey)
		if err != nil {
			jwt_logger.Errorf("Token Signing error: %+v\n", err)
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
