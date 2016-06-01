package main

import (
	"fmt"
	"net/http"
	"github.com/julienschmidt/httprouter"
)

func feedbackSubmission(responseWriter http.ResponseWriter, request *http.Request, requestParameters httprouter.Params) {
	//feedbackMessage := ""
	//testingValue := request.PostFormValue("testing")
	//if testingValue != "" {
		//fmt.Fprint(responseWriter, "{\"success\":\"" + testingValue + "\"}")
	//} else {
		//fmt.Fprint(responseWriter, "{\"error\":\"The message was not sent.\"}")
	//}
	fmt.Fprint(responseWriter, "Nope.")
}