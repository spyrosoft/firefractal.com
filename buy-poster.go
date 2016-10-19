package main

import (
	"strings"
	"fmt"
	"net/http"
	"encoding/json"
	"io/ioutil"
	"github.com/julienschmidt/httprouter"
	"github.com/stripe/stripe-go"
	"github.com/stripe/stripe-go/charge"
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
	chargeParams.SetSource(buyPosterToken)
	chargeResults, error := charge.New(chargeParams)
	if error != nil {
		successMessage.SetMessage(false, error.Error())
		json.NewEncoder(responseWriter).Encode(successMessage)
		return
	}
	json.NewEncoder(responseWriter).Encode(successMessage)
	sendBuyPosterSuccessEmail(request, chargeResults.ID)
}

func validBuyPosterPostVariables(request *http.Request) SuccessMessage {
	successMessage := SuccessMessage{}
	if request.PostFormValue("buy-poster-token") == "" {
		successMessage.Message += "Somehow your payment did not go through. Please fill out your payment details again. "
	}
	if request.PostFormValue("shipping-name") == "" {
		successMessage.Message += "The shipping name field is required. "
	}
	if request.PostFormValue("shipping-address") == "" {
		successMessage.Message += "The shipping address field is required. "
	}
	if request.PostFormValue("shipping-city") == "" {
		successMessage.Message += "The shipping city field is required. "
	}
	if request.PostFormValue("shipping-state") == "" {
		successMessage.Message += "The shipping state field is required. "
	}
	if request.PostFormValue("shipping-zip") == "" {
		successMessage.Message += "The shipping zip code field is required. "
	}
	if request.PostFormValue("shipping-email") == "" {
		successMessage.Message += "The shipping email field is required. "
	}
	if successMessage.Message == "" {
		successMessage.Success = true
	}
	return successMessage
}

func sendBuyPosterSuccessEmail(request *http.Request, orderId string) SuccessMessage {
	successMessage := SuccessMessage{Success: true}
	responseEmailTemplate, error := ioutil.ReadFile("email-templates/order-received.txt")
	if error != nil {
		successMessage.SetMessage(false, "Unable to open email template file.")
	}
	message := searchReplaceResponseEmailTemplate(request, string(responseEmailTemplate))
	sendEmail(request.PostFormValue("shipping-email"), "Receipt From firefractal.com - Order #" + orderId, message)
	sendEmail(credentials.ReplyAddress, "Receipt From firefractal.com - Order #" + orderId, message)
	return successMessage
}

func searchReplaceResponseEmailTemplate(request *http.Request, responseEmailTemplate string) string {
	message := searchReplaceFromForm(request, responseEmailTemplate, "CUSTOMER-NAME", "shipping-name")
	message = searchReplaceFromForm(request, message, "POSTER-SIZE", "poster-size")
	message = searchReplaceFromForm(request, message, "POSTER-ORIENTATION", "poster-orientation")
	message = searchReplaceFromForm(request, message, "SHIPPING-ADDRESS", "shipping-address")
	message = searchReplaceFromForm(request, message, "SHIPPING-CITY", "shipping-city")
	message = searchReplaceFromForm(request, message, "SHIPPING-STATE", "shipping-state")
	message = searchReplaceFromForm(request, message, "SHIPPING-ZIP", "shipping-zip")
	orderTotal := centsToHumanReadableDollars(posterCostInCents[request.PostFormValue("poster-size")])
	message = strings.Replace(message, "ORDER-TOTAL", orderTotal, -1)
	message = searchReplaceFromForm(request, message, "DESTINATION-LINK", "destination-link")
	return message
}

func searchReplaceFromForm(request *http.Request, preSearchReplace string, search string, replace string) string {
	postSearchReplace := strings.Replace(preSearchReplace, search, request.PostFormValue(replace), -1)
	return postSearchReplace
}

func centsToHumanReadableDollars(cents uint64) string {
	dollars := float64(cents) / 100
	return fmt.Sprintf("%5.2f", dollars)
}