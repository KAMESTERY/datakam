package utils

import (
	"bytes"
	"html/template"
	"net/http"
	"net/smtp"
	"strconv"
)

type EmailUser struct {
	Username    string
	Password    string
	EmailServer string
	Port        int
}

type SmtpTemplateData struct {
	From    string
	To      string
	Subject string
	Body    string
}

const emailTemplate = `From: &#123;&#123;.From&#125;&#125;
To: &#123;&#123;.To&#125;&#125;
Subject: &#123;&#123;.Subject&#125;&#125;

&#123;&#123;.Body&#125;&#125;

Sincerely,

&#123;&#123;.From&#125;&#125;
`

func SendEmail(r *http.Request) (string, error) {

	emailUser := &EmailUser{"username", "password", "smtp.gmail.com", 587}
	// emailUser := &EmailUser{"outcastgeek", "SuperPolyglot2012", "smtp.gmail.com", 465}
	// emailUser := &EmailUser{"outcastgeek", "SuperPolyglot2012", "smtp.gmail.com", 25}

	auth := smtp.PlainAuth("",
		emailUser.Username,
		emailUser.Password,
		emailUser.EmailServer,
	)

	var (
		err error
		doc bytes.Buffer
	)

	context := &SmtpTemplateData{
		"SmtpEmailSender",
		"outcastgeek+golang@gmail.com",
		"This is the e-mail subject line!",
		"Hello, this is a test e-mail body.",
	}

	t := template.New("emailTemplate")
	t, err = t.Parse(emailTemplate)
	if err != nil {
		Error(r, "error trying to parse mail template", err)
		return "", err
	}

	err = t.Execute(&doc, context)
	if err != nil {
		Error(r, "error trying to execute mail template", err)
		return "", err
	}

	err = smtp.SendMail(
		emailUser.EmailServer+":"+strconv.Itoa(emailUser.Port), // in our case, "smtp.google.com:587"
		auth,
		emailUser.Username,
		[]string{"outcastgeek@gmail.com"},
		doc.Bytes(),
	)
	if err != nil {
		Error(r, "ERROR: attempting to send a mail ", err)
		return "", err
	}
	return "Email Sent!", nil
}
