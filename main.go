package main

import (
	"fmt"
	"github.com/julienschmidt/httprouter"
	"net/http"
	"log"
)

func feedbackSubmission(responseWriter http.ResponseWriter, request *http.Request, requestParameters httprouter.Params) {
	fmt.Fprint(responseWriter, "{\"error\":\"The message was not sent.\"}")
}

func main() {
	router := httprouter.New()
	router.GET("/feedback", feedbackSubmission)
	router.NotFound = http.FileServer(http.Dir("web-root"))
	log.Fatal(http.ListenAndServe(":8082", router))
}