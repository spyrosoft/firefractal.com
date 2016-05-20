package firefractal

import (
	"github.com/julienschmidt/httprouter"
	"net/http"
	"log"
)

func panicOnError( error error ) { if error != nil { log.Panic( error ) } }

func main() {
	loadCredentials()
	router := httprouter.New()
	router.POST("/feedback", feedbackSubmission)
	router.NotFound = http.FileServer(http.Dir("web-root"))
	log.Fatal(http.ListenAndServe(":8082", router))
}