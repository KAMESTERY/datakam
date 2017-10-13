package resolvers

import (
	"slapman/utils"

	"fmt"
	"github.com/graphql-go/graphql"
	"time"
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

			user_logger.Debugf("Put Args: %+v", p.Args)

			userID := utils.GenerateUUID()
			timeStamp := time.Now().String()

			email := utils.GetStrValue(p.Args, "email")
			if len(email) < 1 {
				return nil, fmt.Errorf("Email Cannot be Empty")
			}

			username := utils.GetStrValue(p.Args, "username")
			if len(username) < 1 {
				return nil, fmt.Errorf("Username Cannot be Empty")
			}

			user := User{
				UserID:    userID,
				Email:     email,
				Username:  username,
				Confirmed: 0,
				Role:      basicUserRole,
				LastSeen:  timeStamp,
			}

			password := utils.GetStrValue(p.Args, "password")
			confirmPassword := utils.GetStrValue(p.Args, "confirmPassword")
			if len(password) < 1 {
				return nil, fmt.Errorf("Password Cannot be Empty")
			}
			if len(confirmPassword) < 1 {
				return nil, fmt.Errorf("Password Confirmation Cannot be Empty")
			}
			if password != confirmPassword {
				return nil, fmt.Errorf("Password/Confirmation Do Not Match")
			}
			passwordHash, err := utils.HashPassword([]byte(password))
			if err != nil {
				return nil, err
			}
			user.PasswordHash = string(passwordHash)

			emailFound, err := emailTaken(p, email)
			if err != nil {
				return nil, err
			}
			if emailFound {
				return nil, fmt.Errorf("This email [%+v] is already taken", email)
			}

			user_logger.Debugf("Persisting User: %+v", user)

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

			_, err = utils.DynaResolvePutItem(p, userTable, user)
			if err != nil {
				return nil, err
			}

			return utils.DynaResolvePutItem(p, userProfileTable, userProfile)
		},
	}
)

func emailTaken(p graphql.ResolveParams, email string) (found bool, err error) {

	index := "EmailIndex"
	params := []interface{}{
		map[string]string{
			"field":     "Email",
			"operation": "EQ",
			"value":     email,
		},
	}
	queryBuilder := utils.DynaQueryDsl(p.Context, userTable, index).Build(params)

	limit, ok := p.Args["limit"].(int)
	if ok && limit > 0 {
		queryBuilder.WithLimit(limit)
		user_logger.Debugf("Limiting Query Results Count to: %+v", limit)
	}

	queryInput, bErr := queryBuilder.AsInput()
	if err = bErr; err != nil {
		return
	}

	count, _, qErr := utils.DynaResolveQuery(p, queryInput)
	if err = qErr; err != nil {
		return
	}

	found = count > 0
	return
}
