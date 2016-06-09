package main

import (
	"net/http"
	"encoding/json"
	"io/ioutil"
	"github.com/julienschmidt/httprouter"
	"github.com/stripe/stripe-go"
)

type SuccessMessage struct {
	Success bool
	Message string
}

func (successMessage *SuccessMessage) SetMessage(success bool, message string) {
	successMessage.Success = success
	successMessage.Message = message
}

var (
	posterCostInCents = map[string]uint64{
		"small": 1500,
		"medium": 4000,
		"large": 5500,
	}
)

//TODO: Break this out into functions
func buyPoster(responseWriter http.ResponseWriter, request *http.Request, requestParameters httprouter.Params) {
	successMessage := validBuyPosterPostVariables(request)
	if ! successMessage.Success {
		json.NewEncoder(responseWriter).Encode(successMessage)
		return
	}
	buyPosterToken := request.PostFormValue("buy-poster-token")
	costInCents := posterCostInCents[request.PostFormValue("poster-size")]
	if credentials.LiveOrDev == "live" {
		stripe.Key = credentials.StripeLiveSecretKey
	} else {
		stripe.Key = credentials.StripeTestSecretKey
	}
	chargeParams := &stripe.ChargeParams{
		Amount: costInCents,
		Currency: "usd",
		Desc: request.PostFormValue("shipping-email") + " " + request.PostFormValue("destination-link"),
	}
	chargeParams.SetSource(request.PostFormValue("buy-poster-token"))
	charge, error := charge.New(chargeParams)
	if error != nil {
		successMessage.SetMessage(false, error)
		json.NewEncoder(responseWriter).Encode(successMessage)
		return
	}
	json.NewEncoder(responseWriter).Encode(successMessage)
}

func validBuyPosterPostVariables(request *http.Request) SuccessMessage {
	successMessage := SuccessMessage{}
	if request.PostFormValue("buy-poster-token") == "" {
		
	}
	return successMessage
}

func sendBuyPosterSuccessEmail(request *http.Request, orderId string) SuccessMessage {
	successMessage := SuccessMessage{Success: true}
	responseEmailTemplate, error := ioutil.ReadFile("/email-templates/order-received.txt")
	if error != nil {
		successMessage.SetMessage(false, "Unable to open email template file.")
	}
	message = responseEmailTemplate
	sendMessage(credentials.ReplyAddress, "Receipt From firefractal.com - Order #" + orderId, message)
	sendMessage(credentials.ReplyAddress, "Feedback Form Submission - firefractal.com", message)
	return successMessage
}