package model

import (
	"slapman/utils"
	"net/http"
	"os"

	"github.com/gorilla/context"
	"github.com/jinzhu/gorm"
)

var driverString, connString string

func init() {
	// Database Connection Strings
	if driverString = os.Getenv("DB_DRIVER"); len(driverString) == 0 {
		driverString = "mysql"
	}
	if connString = os.Getenv("DB_CONN"); len(connString) == 0 {
		connString = "root@cloudsql(outcastgeeksolutions:ogsolsdb)/Ks_Ecom_Prod"
	}
}

//SetDbConn Sets the Database Connection in Http Context
func SetDbConn(r *http.Request, dbConn *gorm.DB) {
	context.Set(r, utils.DbCtxKey, dbConn)
}

//GetDbConn Gets the Database Connection from Http Context
func GetDbConn(r *http.Request) (dbConn *gorm.DB) {
	if dbConn, ok := context.GetOk(r, utils.DbCtxKey); ok {
		return dbConn.(*gorm.DB)
	}
	return
}

//NewDBConn Creates a New Database
func NewDBConn(r *http.Request) *gorm.DB {
	utils.Debug(r, "Connecting to the Database")
	db, err := gorm.Open(driverString, connString)
	if err != nil {
		utils.Errorf(r, "ERROR:::: %+v", err)
		panic("failed to connect database")
	}
	utils.Debugf(r, "Database Connection: %+v", db)
	db.LogMode(true)
	return db
}
