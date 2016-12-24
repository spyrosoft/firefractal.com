package main

import (
	"fmt"
	"net/http"
	"github.com/julienschmidt/httprouter"
)

func feedbackSubmission(responseWriter http.ResponseWriter, request *http.Request, requestParameters httprouter.Params) {
	message := ""
	nameValue := request.PostFormValue("name")
	emailValue := request.PostFormValue("email")
	messageValue := request.PostFormValue("message")
	if emailValue != "" {
		message += "From: " + emailValue + "\r\n\r\n"
	}
	if nameValue != "" {
		message += "Name: " + nameValue + "\r\n\r\n"
	}
	if messageValue != "" {
		message += messageValue
	}
	sendEmail(siteData.ReplyAddress, "Feedback Form Submission - firefractal.com", message)
	fmt.Fprint(responseWriter, "{\"success\":true}")
}