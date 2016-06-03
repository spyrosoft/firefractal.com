package main

import (
	"fmt"
	"net/http"
	"log"
	"strings"
	"os"
	"github.com/julienschmidt/httprouter"
)

type StaticHandler struct {
	http.Dir
}

func (sh *StaticHandler) ServeHttp(responseWriter http.ResponseWriter, request *http.Request) {
	staticFilePath := staticFilePath(request)
	
	fileHandle, error := sh.Open(staticFilePath)
	if error != nil {
		serve404(responseWriter, request)
		return
	}
	defer fileHandle.Close()
	
	fileInfo, error := fileHandle.Stat()
	if error != nil {
		serve404(responseWriter, request)
		return
	}
	
	if fileInfo.IsDir() {
		indexFileHandle, error := sh.Open(staticFilePath + "index.html")
		if error != nil {
			serve404(responseWriter, request)
			return
		}
		return
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
	staticHandler := StaticHandler{"awestruct/_site"}
	
}

func panicOnError(error error) { if error != nil { log.Panic(error) } }

func main() {
	loadCredentials()
	router := httprouter.New()
	router.POST("/feedback", feedbackSubmission)
	router.NotFound = http.HandlerFunc(serveStaticFilesOr404)
	log.Fatal(http.ListenAndServe(":8082", router))
}