package firefractal

import (
	"fmt"
	"github.com/julienschmidt/httprouter"
	"net/http"
	"log"
	"encoding/json"
	"io/ioutil"
)

type Credentials struct {
	AddressName string `json:"no-reply-address-name"`
	Address string `json:"no-reply-address"`
	Password string `json:"no-reply-password"`
	Host string `json:"no-reply-host"`
	Port string `json:"no-reply-host"`
}

func loadCredentials() {
	rawCredentials, error := ioutil.ReadFile( "private/credentials.json" )
	panicOnError( error )
	error = json.Unmarshal( rawCredentials, &credentials )
	panicOnError( error )
}

func feedbackSubmission(responseWriter http.ResponseWriter, request *http.Request, requestParameters httprouter.Params) {
	//feedbackMessage := ""
	fmt.Println(request.PostFormValue("testing"))
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