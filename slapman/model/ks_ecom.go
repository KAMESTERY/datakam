package model

import "slapman/utils"

const (
	slapmanDomain = "slapman"
	slapmanUser   = "shop_user"
)

func init() {
	userTblsMap[slapmanDomain] = slapmanUser
	utils.Debugf(nil, "Added %s to User Table Maps: %+v", slapmanUser, userTblsMap)
}
