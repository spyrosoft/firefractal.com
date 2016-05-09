package main

import (
	"github.com/julienschmidt/httprouter"
	"net/http"
	"log"
)

func main() {
	router := httprouter.New()
	router.NotFound = http.FileServer(http.Dir("web-root"))
	log.Fatal(http.ListenAndServe(":8082", router))
}