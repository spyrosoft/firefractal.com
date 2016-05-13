package main

import (
	"fmt"
	"github.com/julienschmidt/httprouter"
	"net/http"
	"log"
	"encoding/json"
	"io/ioutil"
//	"net/smtp"
)

type Credentials struct {
	NoReplyAddress string `json:"no-reply-address"`
	NoReplyPassword string `json:"no-reply-password"`
	NoReplyHost string `json:"no-reply-host"`
}

var (
	credentials = Credentials{}
)

func loadCredentials() {
	rawCredentials, error := ioutil.ReadFile( "private/credentials.json" )
	panicOnError( error )
	error = json.Unmarshal( rawCredentials, &credentials )
	panicOnError( error )
}

func feedbackSubmission(responseWriter http.ResponseWriter, request *http.Request, requestParameters httprouter.Params) {
	fmt.Fprint(responseWriter, "{\"error\":\"The message was not sent.\"}")
}

func panicOnError( error error ) { if error != nil { panic( error ) } }

func main() {
	loadCredentials()
	router := httprouter.New()
	router.GET("/feedback", feedbackSubmission)
	router.NotFound = http.FileServer(http.Dir("web-root"))
	log.Fatal(http.ListenAndServe(":8082", router))
}