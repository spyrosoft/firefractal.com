package main

import (
	"fmt"
	"github.com/julienschmidt/httprouter"
	"net/http"
	"log"
)

var (
	webRoot = "awestruct/_site"
)

type fileHandlerWithFallback struct {
	directory http.Dir
	fallback http.Handler
}

func (fileHandlerWithFallback *fileHandlerWithFallback) ServeHTTP(responseWriter http.ResponseWriter, request *http.Request) {
	
}

func serveStaticFilesOr404(responseWriter http.ResponseWriter, request *http.Request) {
	fmt.Fprint(responseWriter, "placeholder for 404 html")
}

func panicOnError(error error) { if error != nil { log.Panic(error) } }

func main() {
	loadCredentials()
	router := httprouter.New()
	router.POST("/feedback", feedbackSubmission)
	fileHandlerWithFallback := fileHandlerWithFallback{directory: http.Dir(webRoot), fallback: http.HandlerFunc(serve404)}
	router.NotFound = fileHandlerWithFallback
	log.Fatal(http.ListenAndServe(":8082", router))
}