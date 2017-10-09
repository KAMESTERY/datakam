package model

import (
	"os"
	"slapman/utils"

	"github.com/jinzhu/gorm"
)

var (
	connection_logger        = utils.NewLogger("modelconnection")
	driverString, connString string
)

func init() {
	// Database Connection Strings
	if driverString = os.Getenv("DB_DRIVER"); len(driverString) == 0 {
		driverString = "mysql"
	}
	if connString = os.Getenv("DB_CONN"); len(connString) == 0 {
		connString = "root@cloudsql(outcastgeeksolutions:ogsolsdb)/Ks_Ecom_Prod"
	}
}

//NewDBConn Creates a New Database
func NewDBConn() *gorm.DB {
	connection_logger.Debug("Connecting to the Database")
	db, err := gorm.Open(driverString, connString)
	if err != nil {
		connection_logger.Errorf("ERROR:::: %+v", err)
		panic("failed to connect database")
	}
	connection_logger.Debugf("Database Connection: %+v", db)
	db.LogMode(true)
	return db
}
