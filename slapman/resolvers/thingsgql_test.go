package resolvers

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/suite"
	"golang.org/x/net/context"
)

type ThingGqlTestSuite struct {
	suite.Suite
}

func (tgts *ThingGqlTestSuite) SetupSuite() {

	result, err := loginUser(
		context.Background(),
		"iiii@iiii.iii",
		"iiiiiiii",
	)
	if err != nil {
		thing_logger.Errorf("ERROR:::: %+v", err)
		panic(err)
	}

	thing_logger.Debugf("Login Result: %+v", result)
	//`
	//{
	//  "data": {
	//    "userLogin": {
	//      "token": "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJVc2VySW5mbyI6eyJVc2VyIjp7IlVzZXJJRCI6IiIsIkVtYWlsIjoiIiwiVXNlcm5hbWUiOiIiLCJSb2xlIjowLCJMYXN0U2VlbiI6IiJ9LCJVc2VyUHJvZmlsZSI6eyJVc2VySUQiOiIiLCJBdmF0YXJIYXNoIjoiIiwiTmFtZSI6IiIsIkFnZSI6MCwiQWJvdXRNZSI6IiIsIkxvY2F0aW9uIjoiIiwiTWVtYmVyU2luY2UiOiIifSwiVXNlckdyb3VwcyI6bnVsbH0sImV4cCI6MTUzMDEzOTk5NCwiaXNzIjoiYWRtaW4ifQ.GX3asVj6Y4fsHVB_wiVw5EEaywPp10Q2jMR-2JTsu2VjN8pckltnaR8jM-9vO65DAsJUGrePRwnsl5LAhPMq9KTHTkhCFsuVL6W9ylo5FBkjjRMTtXkuBmUFqIN2Wedx3GYAy21yEV635xYhD-qclluRJF3FAo62a9oigPJNZIg"
	//    }
	//  }
	//}
	//`
}

func TestThingGqlTestSuite(t *testing.T) {
	tgts := new(ThingGqlTestSuite)
	tgts.SetT(t)
	suite.Run(t, tgts)
}

func (tgts *ThingGqlTestSuite) TestLogin() {
	assert.Equal(tgts.T(), 1, 1, "One equals One")
}
