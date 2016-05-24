package firefractal

import (
	"github.com/julienschmidt/httprouter"
	"net/http"
	"log"
)

type fileHandlerWithFallback struct {
	directory http.Dir
	fallback http.Handler
}

func panicOnError( error error ) { if error != nil { log.Panic( error ) } }

func main() {
	loadCredentials()
	router := httprouter.New()
	router.POST("/feedback", feedbackSubmission)
	router.NotFound = http.FileServer(http.Dir("web-root"))
	log.Fatal(http.ListenAndServe(":8082", router))
}