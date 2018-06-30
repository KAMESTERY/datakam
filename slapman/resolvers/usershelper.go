package resolvers

import (
	"os"
	"time"

	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/endpoints"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/dgrijalva/jwt-go"
	"github.com/guregu/dynamo"
	"golang.org/x/net/context"

	"slapman/utils"
)

const (
	userTable           = "User"
	userProfileTable    = "UserProfile"
	userGroupsTable     = "UserGroups"
	basicUserRole       = 1234
	userProfileName     = "What is your name?"
	userProfileAge      = 4321
	userProfileAboutMe  = "What are some words about you?"
	userProfileLocation = "What is your location?"
	userGroupGroupID    = "user"
	userGroupName       = "user"
)

// TODO: Note: Do not embed in User Struct as it creates some issues
type UserRef struct {
	UserID   string `json:"UserID"`
	Email    string `json:"Email"`
	Username string `json:"Username"`
	Role     int64  `json:"Role"`
	LastSeen string `json:"LastSeen`
}

type User struct {
	UserID       string `json:"UserID"`
	Email        string `json:"Email"`
	Username     string `json:"Username"`
	Role         int64  `json:"Role"`
	Confirmed    int    `json:"Confirmed"`
	PasswordHash string `json:"PasswordHash`
	LastSeen     string `json:"LastSeen`
}

func (user *User) asUserRef() (userRef UserRef) {
	userRef.UserID = user.UserID
	userRef.Email = user.Email
	userRef.Username = user.Username
	userRef.Role = user.Role
	userRef.LastSeen = user.LastSeen
	return
}

type UserProfile struct {
	UserID      string `json:"UserID"`
	AvatarHash  string `json:"AvatarHash"`
	Name        string `json:"Name"`
	Age         int    `json:"Age"`
	AboutMe     string `json:"AboutMe"`
	Location    string `json:"Location"`
	MemberSince string `json:"MemberSince"`
}

type UserGroup struct {
	GroupID string `json:"GroupID"`
	UserID  string `json:"UserID"`
	Name    string `json:"Name"`
}

type UserInfo struct {
	User          UserRef     `json:"User"`
	UserProfile   UserProfile `json:"UserProfile"`
	UserGroups    []UserGroup `json:"UserGroups"`
	userOk        bool
	userProfileOk bool
	userGroupsOk  bool
}

func (usri *UserInfo) Ok() bool {
	return usri.userOk && usri.userProfileOk && usri.userGroupsOk
}

func (usri *UserInfo) SetUser(user UserRef) {
	usri.User = user
	usri.userOk = true
	userhelper_logger.Debugf("[UserInfo] User: %+v", user)
}

func (usri *UserInfo) SetUserProfile(userProfile UserProfile) {
	usri.UserProfile = userProfile
	usri.userProfileOk = true
	userhelper_logger.Debugf("[UserInfo] User Profile: %+v", userProfile)
}

func (usri *UserInfo) SetUserGroups(userGroups []UserGroup) {
	usri.UserGroups = userGroups
	usri.userGroupsOk = true
	userhelper_logger.Debugf("[UserInfo] User Groups: %+v", userGroups)
}

const (
	defaultRegion  = endpoints.UsEast1RegionID
	DefaultLimit   = 24
	defaultPageNum = 0
	limitKey       = 17
	pageKey        = 18
)

var (
	userhelper_logger = utils.NewLogger("helperuser")

	sess *session.Session
)

func init() {
	sess = newAwsSession()
}

func loginUser(ctx context.Context, userID, password string) (interface{}, error) {

	db := dynamo.New(sess)
	userOrm := db.Table(userTable)
	userProfileOrm := db.Table(userProfileTable)
	userGroupOrm := db.Table(userGroupsTable)

	var (
		userInfo    UserInfo
		user        User
		userProfile UserProfile
		userGroups  []UserGroup
	)

	if err := userOrm.
		Get("UserID", userID).
		Filter("Password = ?", password).
		OneWithContext(ctx, &user); err != nil {
		userhelper_logger.Debugf("User: %+v", user)
		return nil, err
	}
	userInfo.SetUser(user.asUserRef())

	if err := userProfileOrm.
		Get("UserID", userID).
		Filter("Email = ?", userID).
		OneWithContext(ctx, &userProfile); err != nil {
		userhelper_logger.Debugf("User Profile: %+v", userProfile)
		return nil, err
	}
	userInfo.SetUserProfile(UserProfile{})

	if err := userGroupOrm.Scan().
		Filter("UserID = ?", userID).
		Consistent(true).
		AllWithContext(ctx, &userGroups); err != nil {
		userhelper_logger.Debugf("User Groups: %+v", userGroups)
		return nil, err
	}
	userInfo.SetUserGroups(userGroups)

	userhelper_logger.Debugf("[User Info] %+v", userInfo)

	// Create a signer for rsa 256 and claim c
	c := struct {
		UserInfo UserInfo
		jwt.StandardClaims
	}{
		userInfo,
		jwt.StandardClaims{
			ExpiresAt: time.Now().Add(time.Minute * 20).Unix(),
			Issuer:    "admin",
		},
	}

	return utils.GenerateRsa256JwtToken(c)
}

//NewAwsSession creates a new Session for an AWS Service
func newAwsSession() (sess *session.Session) {

	// Grab Region from Environment
	region := os.Getenv("REGION")
	if len(region) < 1 {
		userhelper_logger.Warnf("WARNING:::: Using Default AWS Region: +%v", defaultRegion)
		region = defaultRegion
	}

	// Create the config specifying the Region for the DynamoDB table.
	// If config.Region is not set the region must come from the shared
	// config or AWS_REGION
	awsCfg := &aws.Config{}
	//awsCfg := aws.NewConfig().WithMaxRetries(5)
	if len(region) > 0 {
		awsCfg.WithRegion(region)
	}
	userhelper_logger.Debugf("AWS Config: %+v", awsCfg)

	// // Create the session that an Aws Service will use
	sess = session.Must(session.NewSession(awsCfg))
	// sess = session.Must(session.NewSessionWithOptions(session.Options{
	// 	SharedConfigState: session.SharedConfigEnable,
	// }))
	userhelper_logger.Debugf("AWS Session: %+v", sess)

	return
}
