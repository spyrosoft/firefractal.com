(load "stripe-credentials.lisp")

(defun send-firefractal-order-confirmation (first-name last-name address city state zip email poster-size poster-orientation destination-link order-total)
  (let ((order-email-message (read-file-into-string "email-templates/order-received.txt")))
    ;;TODO: How can this be kept dry?
    (setq order-email-message (regex-replace "CUSTOMER-NAME" order-email-message (concatenate 'string first-name " " last-name)))
    (setq order-email-message (regex-replace "POSTER-SIZE" order-email-message poster-size))
    (setq order-email-message (regex-replace "POSTER-ORIENTATION" order-email-message poster-orientation))
    (setq order-email-message (regex-replace "ORDER-TOTAL" order-email-message order-total))
    (setq order-email-message (regex-replace "SHIPPING-ADDRESS" order-email-message address))
    (setq order-email-message (regex-replace "SHIPPING-CITY" order-email-message city))
    (setq order-email-message (regex-replace "SHIPPING-STATE" order-email-message state))
    (setq order-email-message (regex-replace "SHIPPING-ZIP" order-email-message zip))
    (setq order-email-message (regex-replace "DESTINATION-LINK" order-email-message destination-link))
    (email-mailgun-message *firefractal-from-email-address* "Order Receipt - firefractal.com" order-email-message)
    (email-mailgun-message email "Order Receipt - firefractal.com" order-email-message)
  ))

(define-easy-handler (buy-poster
                      :uri "/buy-poster/"
                      :default-request-type :post)
    (shipping-first-name shipping-last-name shipping-address shipping-city shipping-state shipping-zip shipping-email poster-size poster-orientation destination-link order-total buy-poster-token)
  (let ((cost-in-cents 0) (payment-gateway-response))
    (cond ((string-equal poster-size "small")
           (setq cost-in-cents "1500"))
          ((string-equal poster-size "medium")
           (setq cost-in-cents "4000"))
          ((string-equal poster-size "large")
           (setq cost-in-cents "5500")))
    (setq payment-gateway-response (nth-value 7 (drakma:http-request
     "https://api.stripe.com/v1/charges"
     :method :post
     :basic-authorization (list *stripe-test-publishable-key* "")
     :parameters (list (cons "amount" cost-in-cents)
                       (cons "currency" "usd")
                       (cons "source" buy-poster-token)
                       (cons "description" (concatenate 'string shipping-email " " destination-link))))))
    (when (string-equal "OK" payment-gateway-response)
      (send-firefractal-order-confirmation shipping-first-name shipping-last-name shipping-address shipping-city shipping-state shipping-zip shipping-email poster-size poster-orientation destination-link order-total))
    (concatenate 'string "{\"response\":\"" payment-gateway-response "\"}")
  ))