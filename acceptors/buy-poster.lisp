(defun cost-of-poster-in-cents (poster-size)
  (cond ((string-equal poster-size "small") "1500")
        ((string-equal poster-size "medium") "4000")
        ((string-equal poster-size "large") "5500")))

(defun create-a-payment (cost-in-cents buy-poster-token shipping-email destination-link)
  (let ((stripe-key
         (if (eq *live-or-dev* 'dev)
             *stripe-test-secret-key*
             *stripe-live-secret-key*)))
    (flexi-streams:octets-to-string
   (drakma:http-request
    "https://api.stripe.com/v1/charges"
    :method :post
    :basic-authorization (list stripe-key "")
    :parameters (list (cons "amount" cost-in-cents)
                      (cons "currency" "usd")
                      (cons "source" buy-poster-token)
                      (cons "description"
                            (concatenate 'string
                                         shipping-email
                                         " "
                                         destination-link)))))))

(defun send-firefractal-order-confirmation (first-name last-name address city state zip email poster-size poster-orientation destination-link order-total order-id)
  (let ((order-email-message (read-file-into-string "email-templates/order-received.txt")))
    ;;TODO: How can we keep this dry?
    ;;(mapcar)?
    (setq order-email-message (cl-ppcre:regex-replace "CUSTOMER-NAME" order-email-message (concatenate 'string first-name " " last-name)))
    (setq order-email-message (cl-ppcre:regex-replace "POSTER-SIZE" order-email-message poster-size))
    (setq order-email-message (cl-ppcre:regex-replace "POSTER-ORIENTATION" order-email-message poster-orientation))
    (setq order-email-message (cl-ppcre:regex-replace "ORDER-TOTAL" order-email-message order-total))
    (setq order-email-message (cl-ppcre:regex-replace "SHIPPING-ADDRESS" order-email-message address))
    (setq order-email-message (cl-ppcre:regex-replace "SHIPPING-CITY" order-email-message city))
    (setq order-email-message (cl-ppcre:regex-replace "SHIPPING-STATE" order-email-message state))
    (setq order-email-message (cl-ppcre:regex-replace "SHIPPING-ZIP" order-email-message zip))
    (setq order-email-message (cl-ppcre:regex-replace "DESTINATION-LINK" order-email-message destination-link))
    (mailgun:send-message email (concatenate 'string "Receipt From firefractal.com - Order #" order-id) order-email-message :bcc *firefractal-from-email-address*)
    ))

(define-easy-handler (buy-poster
                      :uri "/buy-poster/"
                      :default-request-type :post)
    (shipping-first-name shipping-last-name shipping-address shipping-city shipping-state shipping-zip shipping-email poster-size poster-orientation destination-link order-total buy-poster-token)
  (let* ((cost-in-cents (cost-of-poster-in-cents poster-size))
         (payment-gateway-response
          (create-a-payment cost-in-cents buy-poster-token shipping-email destination-link))
         (payment-gateway-response-json
          (jsown:parse payment-gateway-response))
         (order-id)
         (error-message))
    (handler-case (setq order-id (jsown:val payment-gateway-response-json "id"))
      (error nil
        (handler-case (setq error-message (jsown:val (jsown:val payment-gateway-response-json "error") "message"))
          (error nil (setq error-message "Unable to connect to the payment gateway.")))))
    (if order-id
        (progn
          (send-firefractal-order-confirmation shipping-first-name shipping-last-name shipping-address shipping-city shipping-state shipping-zip shipping-email poster-size poster-orientation destination-link order-total order-id)
          "{\"success\":\"true\"}")
        (concatenate 'string "{\"success\":\"false\",\"message\":\""
                     error-message
                     "\"}"))
    ))