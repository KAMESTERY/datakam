package model

import (
	"slapman/utils"
	kscrypto "slapman/utils/crypto"
)

var (
	user_logger = utils.NewLogger("modeluser")
	userTblsMap = make(map[string]string)
)

// UserRef struct to be used for JWT Bearer Token
type UserRef struct {
	Domain   string `db:"domain" json:"domain" gorm:"not null;unique"`
	Username string `db:"username" json:"username" gorm:"not null;unique"`
	Email    string `db:"email" json:"email"`
}

// GetUserTable Get actual User Table from Domain
func (uref *UserRef) GetUserTable() (userTable string) {
	userTable = userTblsMap[uref.Domain]
	user_logger.Debugf("Retrieved User Table: %s from %+v with Domain: %s", userTable, userTblsMap, uref.Domain)
	return
}

// User struct for parsing login credentials
type User struct {
	UserRef
	ID        uint64 `db:"id" json:"id" gorm:"AUTO_INCREMENT"`
	Password  string `db:"password" json:"password" gorm:"not null"`
	Active    bool   `db:"active" json:"active"`
	LastLogin string `db:"last_login" json:"last_login"`
}

// Save Persists New User to the Database
func (user *User) Save() (ok bool) {
	if cryptedPassBytes, err := kscrypto.HashPassword([]byte(user.Password)); err == nil {
		cryptedPass := string(cryptedPassBytes)
		newUser := &User{
			UserRef{
				user.Domain,
				user.Username,
				user.Email,
			},
			user.ID,
			cryptedPass,
			user.Active,
			user.LastLogin,
		}
		db := NewDBConn()
		defer db.Close()
		db.Table(newUser.GetUserTable()).Save(newUser)
		ok = true
	} else {
		user_logger.Errorf("ERROR:::: %+v", err)
	}
	return
}

// Ok validates User
func (user *User) Ok() (ok bool) {
	if user.Domain != "" && user.Password != "" &&
		(user.Username != "" || user.Email != "") {
		ok = user.Active // Only Active Users are Ok
	}
	return
}

// FindUser Finds an existing User in the Database
func (user *User) FindUser() (userFound *User) {
	userFound = &User{}
	db := NewDBConn()
	defer db.Close()
	db.Table(user.GetUserTable()).Where(user).First(userFound)
	return
}

// Verify Verifies User for Authentication
func (user *User) Verify() (userRef *UserRef) {
	if !user.Ok() {
		user_logger.Errorf("BAD USER CREDENTIALS:::: %+v", user)
	}

	// validate user credentials
	if userFound := user.FindUser(); userFound.Ok() {
		if cryptedPassBytes, err := kscrypto.HashPassword([]byte(user.Password)); err != nil {
			user_logger.Errorf("ERROR:::: %+v", err)
		} else {
			if err := kscrypto.CheckPasswordHash([]byte(userFound.Password), cryptedPassBytes); err != nil {
				user_logger.Errorf("ERROR:::: %+v", err)
			} else {
				userRef = userFound.GetRef()
			}
		}
	}
	return
}

//UserIn struct for parsing registration information
type UserIn struct {
	User
	PasswordConfirmation string `json:"passconfimation" gorm:"not null"`
}

// Ok validates UserIn
func (userIn *UserIn) Ok() (ok bool) {
	if userIn.Domain != "" && userIn.Password != "" &&
		userIn.Username != "" && userIn.Email != "" &&
		userIn.Password == userIn.PasswordConfirmation {
		ok = true
	}
	return
}

// GetRef returns a UserRef
func (user *User) GetRef() *UserRef {
	return &UserRef{
		user.Domain,
		user.Username,
		user.Email,
	}
}
