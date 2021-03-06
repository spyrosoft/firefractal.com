package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"strings"

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
	printCostInCents = map[string]uint64{
		"11x8.5": 7500,
		"24x18":  20000,
		"32x16":  20000,
		"48x16":  40000,
		"60x30":  60000,
	}
)

func buyPrint(responseWriter http.ResponseWriter, request *http.Request, requestParameters httprouter.Params) {
	successMessage := validBuyPrintPostVariables(request)
	if !successMessage.Success {
		json.NewEncoder(responseWriter).Encode(successMessage)
		return
	}
	buyPrintToken := request.PostFormValue("buy-print-token")
	costInCents := printCostInCents[request.PostFormValue("print-size")]
	if siteData.LiveOrDev == "live" {
		stripe.Key = siteData.StripeLiveSecretKey
	} else {
		stripe.Key = siteData.StripeTestSecretKey
	}
	chargeParams := &stripe.ChargeParams{
		Amount:   costInCents,
		Currency: "usd",
		Desc:     request.PostFormValue("shipping-email") + " " + request.PostFormValue("destination-link"),
	}
	chargeParams.SetSource(buyPrintToken)
	chargeResults, error := charge.New(chargeParams)
	if error != nil {
		successMessage.SetMessage(false, error.Error())
		json.NewEncoder(responseWriter).Encode(successMessage)
		return
	}
	json.NewEncoder(responseWriter).Encode(successMessage)
	sendBuyPrintSuccessEmail(request, chargeResults.ID)
}

func validBuyPrintPostVariables(request *http.Request) SuccessMessage {
	successMessage := SuccessMessage{}
	if request.PostFormValue("buy-print-token") == "" {
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

func sendBuyPrintSuccessEmail(request *http.Request, orderId string) SuccessMessage {
	successMessage := SuccessMessage{Success: true}
	responseEmailTemplate, error := ioutil.ReadFile("email-templates/order-received.txt")
	if error != nil {
		successMessage.SetMessage(false, "Unable to open email template file.")
	}
	message := searchReplaceResponseEmailTemplate(request, string(responseEmailTemplate))
	sendEmail(request.PostFormValue("shipping-email"), "Receipt From firefractal.com - Order #"+orderId, message)
	sendEmail(siteData.ReplyAddress, "Receipt From firefractal.com - Order #"+orderId, message)
	return successMessage
}

func searchReplaceResponseEmailTemplate(request *http.Request, responseEmailTemplate string) string {
	message := searchReplaceFromForm(request, responseEmailTemplate, "CUSTOMER-NAME", "shipping-name")
	message = searchReplaceFromForm(request, message, "PRINT-SIZE", "print-size")
	message = searchReplaceFromForm(request, message, "PRINT-ORIENTATION", "print-orientation")
	message = searchReplaceFromForm(request, message, "SHIPPING-ADDRESS", "shipping-address")
	message = searchReplaceFromForm(request, message, "SHIPPING-CITY", "shipping-city")
	message = searchReplaceFromForm(request, message, "SHIPPING-STATE", "shipping-state")
	message = searchReplaceFromForm(request, message, "SHIPPING-ZIP", "shipping-zip")
	orderTotal := centsToHumanReadableDollars(printCostInCents[request.PostFormValue("print-size")])
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
