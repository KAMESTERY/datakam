package resolvers

import (
	"slapman/utils"
	"slapman/utils/group"

	"fmt"
	"time"

	"github.com/graphql-go/graphql"
)

const (
	userTable           = "User"
	userProfileTable    = "UserProfile"
	basicUserRole       = 1234
	userProfileName     = "What is your name?"
	userProfileAge      = 4321
	userProfileAboutMe  = "What are some words about you?"
	userProfileLocation = "What is your location?"
)

type User struct {
	UserID       string `json:"UserID"`
	Email        string `json:"Email"`
	Username     string `json:"Username"`
	Role         int64  `json:"Role"`
	Confirmed    int    `json:"Confirmed"`
	PasswordHash string `json:"PasswordHash`
	LastSeen     string `json:"LastSeen`
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

var (
	user_logger = utils.NewLogger("resolversuser")

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
						PasswordHash: string(passwordHash),
						Confirmed:    0,
						Role:         basicUserRole,
						LastSeen:     timeStamp,
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
