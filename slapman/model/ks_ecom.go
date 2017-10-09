package model

import (
	"slapman/utils"
)

const (
	slapmanDomain = "slapman"
	slapmanUser   = "shop_user"
)

var ksecom_logger = utils.NewLogger("modelksecom")

func init() {
	userTblsMap[slapmanDomain] = slapmanUser
	ksecom_logger.Debugf("Added %s to User Table Maps: %+v", slapmanUser, userTblsMap)
}
