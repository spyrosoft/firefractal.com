package main

import (
	"fmt"
	"net/http"
	"encoding/json"
	"github.com/julienschmidt/httprouter"
)

type SuccessMessage struct {
	Success bool
	Message string
}

func buyPoster(responseWriter http.ResponseWriter, request *http.Request, requestParameters httprouter.Params) {
	if validBuyPosterRequest(request) {
		
	} else {
		
	}
	
}

func sendBuyPosterSuccessEmail(request *http.Request) {
	successMessage := SuccessMessage{Success: true}
	responseEmailTemplate, error := ioutil.ReadFile("/email-templates/order-received.txt")
	if error != nil {
		successMessage.Success = false
	}
	"Receipt From firefractal.com - Order #"
	sendMessage(credentials.ReplyAddress, , message)
	sendMessage(credentials.ReplyAddress, "Feedback Form Submission - firefractal.com", message)
}

func validPosterBuyPoster(request *http.Request) bool {
	valid := true
	
	return valid
}