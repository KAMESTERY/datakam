package resolvers

import (
	"slapman/utils"
	"slapman/utils/group"

	"fmt"
	"time"

	"github.com/dgrijalva/jwt-go"
	"github.com/graphql-go/graphql"
	"github.com/mitchellh/mapstructure"
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
)

// Note: Do not embed in User Struct as it creates some issues
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
	User        UserRef
	UserProfile UserProfile
	UserGroups  []UserGroup
}

var (
	user_logger = utils.NewLogger("resolversuser")

	// JwtTokenType represents a JWT Token
	JwtTokenType = graphql.NewObject(graphql.ObjectConfig{
		Name: "JwtToken",
		Fields: graphql.Fields{
			"token": &graphql.Field{
				Type:        graphql.NewNonNull(graphql.String),
				Description: "The JWT Token",
			},
		},
	})

	// UserJwtTokenRefreshFields represents the parameters and the resolver function to Refrsh a user JWT Token
	UserJwtTokenRefreshFields = graphql.Field{
		Type:        JwtTokenType,
		Description: "Refreshes the JWT Token for a user",
		Args: graphql.FieldConfigArgument{
			"token": &graphql.ArgumentConfig{
				Type: graphql.NewNonNull(graphql.String),
			},
		},
		Resolve: func(p graphql.ResolveParams) (interface{}, error) {

			user_logger.Debugf("Token Refresh Args: %+v", p.Args)

			tokenString := p.Args["token"].(string)
			if tokenString == "" {
				return nil, fmt.Errorf("Token Cannot be Empty")
			}

			newToken, claims, err := utils.RefreshRsa256JwtToken(tokenString)
			if err != nil {
				return nil, err
			}

			// Update the LastSeen TimeStamp for the Current User
			var userClaim struct {
				User UserRef
			}
			mapstructure.Decode(claims, &userClaim)

			user_logger.Debugf("User Claims: [%+v]", claims)
			user_logger.Debugf("Updating [LastSeen] Property for User: [%+v]", userClaim)

			keyData := map[string]interface{}{
				"UserID": userClaim.User.UserID,
				"Email":  userClaim.User.Email,
			}
			data := map[string]interface{}{
				"LastSeen": time.Now().String(),
			}
			_, err = utils.DynaResolveUpdateItem(p, userTable, keyData, data)
			if err != nil {
				user_logger.Errorf("ERROR:::: %+v", err) // Log the error and Proceed
			}

			return newToken, nil
		},
	}

	// UserLoginFields represents the parameters and the resolver function to Login a user from the DynamoDB User Table
	UserLoginFields = graphql.Field{
		Type:        JwtTokenType,
		Description: "Creates a new entry in DynamoDB User Table",
		Args: graphql.FieldConfigArgument{
			"region": &graphql.ArgumentConfig{
				Type: graphql.String,
			},
			"limit": &graphql.ArgumentConfig{
				Type: graphql.Int,
			},
			"email": &graphql.ArgumentConfig{
				Type: graphql.NewNonNull(graphql.String),
			},
			"password": &graphql.ArgumentConfig{
				Type: graphql.NewNonNull(graphql.String),
			},
		},
		Resolve: func(p graphql.ResolveParams) (interface{}, error) {

			user_logger.Debugf("Login User Args: %+v", p.Args)

			email := p.Args["email"].(string)
			if email == "" {
				return nil, fmt.Errorf("Email Cannot be Empty")
			}
			userID := email // The user ID serves as the UserID

			password := p.Args["password"].(string)
			if password == "" {
				return nil, fmt.Errorf("Password Cannot be Empty")
			}

			keyData := map[string]interface{}{
				"UserID": userID,
				"Email":  email,
			}

			userInfo := UserInfo{}

			var g group.Group
			{
				// Create User
				g.Add(func() (err error) {
					foundRecord, getErr := utils.DynaResolveGetItem(p, userTable, keyData)
					if getErr != nil {
						err = fmt.Errorf("Could not log you in: %+v", getErr)
						return
					}
					user_logger.Debugf("Found Record: %+v", foundRecord)

					var foundUser User
					mapstructure.Decode(foundRecord, &foundUser)

					user_logger.Debugf("Found User: %+v", foundUser)

					// Checking user password
					if checkErr := utils.CheckPasswordHash([]byte(foundUser.PasswordHash), []byte(password)); err != nil {
						user_logger.Errorf("ERROR:::: %+v", checkErr)
						err = fmt.Errorf("Wrong Email/Password Provided")
						return
					}

					user_logger.Debugf("Logging in user with email: [%+v]", email)

					userInfo.User = foundUser.asUserRef()

					return
				}, func(err error) {
					user_logger.Errorf("ERROR:::: %+v", err)
				})
			}
			{
				// Retrieve UserProfile
				g.Add(func() (err error) {
					queryInput, dslErr := utils.
						DynaQueryDsl(p.Context, userProfileTable, "UserIDIndex").
						WithParam("UserID", "EQ", userID).AsInput()
					if dslErr != nil {
						user_logger.Errorf("Could not rerieve User Group: %+v", err)
						err = dslErr
						return
					}
					foundRecord, getErr := utils.DynaResolveOneQuery(p, queryInput)
					if getErr != nil {
						user_logger.Errorf("Could not retrieve User Profile: %+v", getErr)
						err = getErr
						return
					}
					var foundUserProfile UserProfile
					mapstructure.Decode(foundRecord, &foundUserProfile)
					userInfo.UserProfile = foundUserProfile
					return
				}, func(err error) {
					user_logger.Errorf("ERROR:::: %+v", err)
				})
			}
			{
				// Retrieve UserGroup
				g.Add(func() (err error) {
					queryInput, dslErr := utils.
						DynaQueryDsl(p.Context, userGroupsTable, "UserIDIndex").
						WithParam("UserID", "EQ", userID).AsInput()
					if dslErr != nil {
						user_logger.Errorf("Could not rerieve User Group: %+v", err)
						err = dslErr
					}
					_, rows, qErr := utils.DynaResolveQuery(p, queryInput)
					if err != nil {
						user_logger.Errorf("Could not log you in: %+v", err)
						err = qErr
					}
					for row := range rows {
						var foundUserGroup UserGroup
						mapstructure.Decode(row, &foundUserGroup)
						userInfo.UserGroups = append(userInfo.UserGroups, foundUserGroup)
					}
					return
				}, func(err error) {
					user_logger.Errorf("ERROR:::: %+v", err)
				})
			}

			err := g.Run()
			if err != nil {
				user_logger.Errorf("ERROR:::: %+v", err)
				return nil, err
			}

			// Create a signer for rsa 256 and claim c
			c := struct {
				User UserInfo
				jwt.StandardClaims
			}{
				userInfo,
				jwt.StandardClaims{
					ExpiresAt: time.Now().Add(time.Minute * 20).Unix(),
					Issuer:    "admin",
				},
			}

			return utils.GenerateRsa256JwtToken(c)
		},
	}

	// UserCreateFields represents the parameters and the resolver function to create a new entry in DynamoDB User Table
	UserCreateFields = graphql.Field{
		Type:        graphql.String,
		Description: "Creates a new entry in DynamoDB User Table",
		Args: graphql.FieldConfigArgument{
			"region": &graphql.ArgumentConfig{
				Type: graphql.String,
			},
			"limit": &graphql.ArgumentConfig{
				Type: graphql.Int,
			},
			"email": &graphql.ArgumentConfig{
				Type: graphql.NewNonNull(graphql.String),
			},
			"username": &graphql.ArgumentConfig{
				Type: graphql.NewNonNull(graphql.String),
			},
			"password": &graphql.ArgumentConfig{
				Type: graphql.NewNonNull(graphql.String),
			},
			"confirmPassword": &graphql.ArgumentConfig{
				Type: graphql.NewNonNull(graphql.String),
			},
		},
		Resolve: func(p graphql.ResolveParams) (interface{}, error) {

			user_logger.Debugf("Create User Args: %+v", p.Args)

			//userID := utils.GenerateUUID()
			timeStamp := time.Now().String()

			email := p.Args["email"].(string)
			if email == "" {
				return nil, fmt.Errorf("Email Cannot be Empty")
			}
			userID := email // The user ID serves as the UserID

			emailFound, err := emailTaken(p, email)
			if err != nil {
				return nil, err
			}
			if emailFound {
				return nil, fmt.Errorf("This email [%+v] is already taken", email)
			}

			var g group.Group
			{
				// Create User
				g.Add(func() (err error) {

					username := p.Args["username"].(string)
					if username == "" {
						err = fmt.Errorf("Username Cannot be Empty")
					}
					password := p.Args["password"].(string)
					confirmPassword := p.Args["confirmPassword"].(string)
					if password == "" {
						err = fmt.Errorf("Password Cannot be Empty")
					}
					if confirmPassword == "" {
						err = fmt.Errorf("Password Confirmation Cannot be Empty")
					}
					if password != confirmPassword {
						err = fmt.Errorf("Password/Confirmation Do Not Match")
					}

					user_logger.Debug("Hashing password...")
					passwordHash, err := utils.HashPassword([]byte(password))
					if err != nil {
						return
					}
					user_logger.Debug("Done Hashing password")

					user := User{
						UserID:       userID,
						Email:        email,
						Username:     username,
						Role:         basicUserRole,
						LastSeen:     timeStamp,
						PasswordHash: string(passwordHash),
						Confirmed:    0,
					}

					user_logger.Debugf("Persisting User: %+v", user)

					_, err = utils.DynaResolvePutItem(p, userTable, user)
					if err != nil {
						return
					}

					return
				}, func(err error) {
					user_logger.Errorf("ERROR:::: %+v", err)
				})
			}
			{
				// Create User Profile
				g.Add(func() (err error) {

					avatartHash := utils.HashStringMD5(email)

					userProfile := UserProfile{
						UserID:      userID,
						AvatarHash:  avatartHash,
						Name:        userProfileName,
						Age:         userProfileAge,
						AboutMe:     userProfileAboutMe,
						Location:    userProfileLocation,
						MemberSince: timeStamp,
					}

					user_logger.Debugf("Persisting User Profile: %+v", userProfile)

					_, err = utils.DynaResolvePutItem(p, userProfileTable, userProfile)
					if err != nil {
						return
					}

					return
				}, func(err error) {
					user_logger.Errorf("ERROR:::: %+v", err)
				})
			}

			err = g.Run()
			if err != nil {
				user_logger.Errorf("ERROR:::: %+v", err)
				return nil, err
			}

			return "success", nil
		},
	}
)

func emailTaken(p graphql.ResolveParams, email string) (found bool, err error) {

	user_logger.Debugf("Checking use of Email: [%+v]...", email)

	keyData := map[string]interface{}{
		"UserID": email,
		"Email":  email,
	}

	foundRecord, getErr := utils.DynaResolveGetItem(p, userTable, keyData)
	if getErr != nil {
		err = fmt.Errorf("Could not check for existing email: %+v", getErr)
		return
	}

	user_logger.Debugf("Found existing Record: %+v with email: %+v", foundRecord, email)

	found = foundRecord != nil
	return
}
