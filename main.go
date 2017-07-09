package main

import (
	"fmt"
	"log"
	"net/http"

	"github.com/julienschmidt/httprouter"
)

type SiteData struct {
	LiveOrDev             string            `json:"live-or-dev"`
	URLPermanentRedirects map[string]string `json:"url-permanent-redirects"`
	NoReplyAddressName    string            `json:"no-reply-address-name"`
	NoReplyAddress        string            `json:"no-reply-address"`
	NoReplyPassword       string            `json:"no-reply-password"`
	Host                  string            `json:"no-reply-host"`
	Port                  string            `json:"no-reply-port"`
	ReplyAddress          string            `json:"reply-address"`
	StripeTestSecretKey   string            `json:"stripe-test-secret-key"`
	StripeLiveSecretKey   string            `json:"stripe-live-secret-key"`
	WebPort               string            `json:"web-port"`
}

var (
	webRoot        = "awestruct/_site"
	siteData       = SiteData{}
	siteDataLoaded = false
)

func main() {
	loadSiteData()
	router := httprouter.New()
	router.POST("/feedback", feedbackSubmission)
	router.POST("/buy-print", buyPrint)
	router.POST("/donate", donate)
	router.GET("/png", png)
	router.NotFound = http.HandlerFunc(requestCatchAll)
	log.Fatal(http.ListenAndServe(":"+siteData.WebPort, router))
}

func dump(things ...interface{}) {
	if siteData.LiveOrDev == "dev" {
		fmt.Println("====================")
		for _, thing := range things {
			fmt.Printf("%+v\n", thing)
		}
		fmt.Println("^^^^^^^^^^^^^^^^^^^^")
	}
}

func dumpType(thing interface{}) {
	fmt.Printf("%T\n", thing)
}
