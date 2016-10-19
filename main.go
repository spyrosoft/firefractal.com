package main

import (
	"fmt"
	"net/http"
	"log"
	"strings"
	"path"
	"io/ioutil"
	"encoding/json"
	"github.com/julienschmidt/httprouter"
)

type Credentials struct {
	LiveOrDev string `json:"live-or-dev"`
	NoReplyAddressName string `json:"no-reply-address-name"`
	NoReplyAddress string `json:"no-reply-address"`
	NoReplyPassword string `json:"no-reply-password"`
	Host string `json:"no-reply-host"`
	Port string `json:"no-reply-port"`
	ReplyAddress string `json:"reply-address"`
	StripeTestSecretKey string `json:"stripe-test-secret-key"`
	StripeTestPublishableKey string `json:"stripe-test-publishable-key"`
	StripeLiveSecretKey string `json:"stripe-live-secret-key"`
	StripeLivePublishableKey string `json:"stripe-live-publishable-key"`
}

type StaticHandler struct {
	http.Dir
}

var (
	webRoot = "awestruct/_site"
	credentials = Credentials{}
	credentialsHaveBeenLoaded = false
)

func loadCredentials() {
	rawCredentials, error := ioutil.ReadFile("private/credentials.json")
	panicOnError(error)
	error = json.Unmarshal(rawCredentials, &credentials)
	panicOnError(error)
	credentialsHaveBeenLoaded = true
}

func (sh *StaticHandler) ServeHttp(responseWriter http.ResponseWriter, request *http.Request) {
	staticFilePath := staticFilePath(request)
	
	fileHandle, error := sh.Open(staticFilePath)
	if serve404OnError(error, responseWriter) { return }
	defer fileHandle.Close()
	
	fileInfo, error := fileHandle.Stat()
	if serve404OnError(error, responseWriter) { return }
	
	if fileInfo.IsDir() {
		fileHandle, error = sh.Open(staticFilePath + "index.html")
		if serve404OnError(error, responseWriter) { return }
		defer fileHandle.Close()
		
		fileInfo, error = fileHandle.Stat()
		if serve404OnError(error, responseWriter) { return }
	}
	
	http.ServeContent(responseWriter, request, fileInfo.Name(), fileInfo.ModTime(), fileHandle)
}

func staticFilePath(request *http.Request) string {
	staticFilePath := request.URL.Path
	if !strings.HasPrefix(staticFilePath, "/") {
		staticFilePath = "/" + staticFilePath
		request.URL.Path = staticFilePath
	}
	return path.Clean(staticFilePath)
}

func serveStaticFilesOr404(responseWriter http.ResponseWriter, request *http.Request) {
	staticHandler := StaticHandler{http.Dir(webRoot)}
	staticHandler.ServeHttp(responseWriter, request)
}

func serve404OnError(error error, responseWriter http.ResponseWriter) bool {
	if error != nil {
		//TODO: There must be a better way:
		responseWriter.WriteHeader(http.StatusNotFound)
		errorTemplate404Content, error := ioutil.ReadFile(webRoot + "/error-templates/404.html")
		panicOnError(error)
		fmt.Fprint(responseWriter, string(errorTemplate404Content))
		return true
	}
	return false
}

func panicOnError(error error) { if error != nil { log.Panic(error) } }

func main() {
	loadCredentials()
	router := httprouter.New()
	router.POST("/feedback", feedbackSubmission)
	router.POST("/buy-poster", buyPoster)
	router.NotFound = http.HandlerFunc(serveStaticFilesOr404)
	log.Fatal(http.ListenAndServe(":8082", router))
}