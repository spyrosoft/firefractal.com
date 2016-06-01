package main

import (
	"crypto/tls"
	"fmt"
	"log"
	"net/mail"
	"net/smtp"
	"encoding/json"
	"io/ioutil"
)

type Credentials struct {
	NoReplyAddressName string `json:"no-reply-address-name"`
	NoReplyAddress string `json:"no-reply-address"`
	NoReplyPassword string `json:"no-reply-password"`
	Host string `json:"no-reply-host"`
	Port string `json:"no-reply-host"`
}

func loadCredentials() {
	rawCredentials, error := ioutil.ReadFile("private/credentials.json")
	panicOnError(error)
	error = json.Unmarshal(rawCredentials, &credentials)
	panicOnError(error)
}

var (
	credentials = Credentials{}
	credentialsHaveBeenSet = false
)

func setCredentials(address string, password string, host string, port string) {
	credentials.NoReplyAddress = address
	credentialsHaveBeenSet = true
}

func sendMessage(recipientAddress string, subject string, messageBody string) {
	if ! credentialsHaveBeenSet {
		log.Panic("Outgoing email credentials have not been set. Cannot send message.")
	}
	
	from := mail.Address{credentials.NoReplyAddressName, credentials.NoReplyAddress}

	headers := make(map[string]string)
	headers["From"] = from.String()
	headers["To"] = recipientAddress
	headers["Subject"] = subject

	message := ""
	for headerName, headerValue := range headers {
		message += fmt.Sprintf("%s: %s\r\n", headerName, headerValue)
	}
	message += "\r\n" + messageBody

	mailAuth := smtp.PlainAuth("", credentials.NoReplyAddress, credentials.NoReplyPassword, credentials.Host)

	tlsConfig := &tls.Config{
		InsecureSkipVerify: true,
		ServerName: credentials.Host,
	}

	tcpConnection, error := tls.Dial("tcp", credentials.Host + ":" + credentials.Port, tlsConfig)
	panicOnError(error)

	smtpClient, error := smtp.NewClient(tcpConnection, credentials.Host)
	panicOnError(error)

	error = smtpClient.Auth(mailAuth)
	panicOnError(error)

	error = smtpClient.Mail(credentials.NoReplyAddress)
	panicOnError(error)

	error = smtpClient.Rcpt(recipientAddress)
	panicOnError(error)

	emailStream, error := smtpClient.Data()
	panicOnError(error)

	_, error = emailStream.Write([]byte(message))
	panicOnError(error)

	error = emailStream.Close()
	panicOnError(error)

	smtpClient.Quit()
}