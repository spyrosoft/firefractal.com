(load "stripe-credentials.lisp")

(defun cost-of-poster-in-cents (poster-size)
  (cond ((string-equal poster-size "small") "1500")
        ((string-equal poster-size "medium") "4000")
        ((string-equal poster-size "large") "5500")))

(defun create-a-payment (cost-in-cents buy-poster-token shipping-email destination-link)
  (flexi-streams:octets-to-string
   (drakma:http-request
    "https://api.stripe.com/v1/charges"
    :method :post
    :basic-authorization (list *stripe-live-secret-key* "")
    :parameters (list (cons "amount" cost-in-cents)
                      (cons "currency" "usd")
                      (cons "source" buy-poster-token)
                      (cons "description"
                            (concatenate 'string
                                         shipping-email
                                         " "
                                         destination-link))))))

(defun send-firefractal-order-confirmation (first-name last-name address city state zip email poster-size poster-orientation destination-link order-total order-id)
  (let ((order-email-message (read-file-into-string "email-templates/order-received.txt")))
    ;;TODO: How can we keep this dry?
    (setq order-email-message (regex-replace "CUSTOMER-NAME" order-email-message (concatenate 'string first-name " " last-name)))
    (setq order-email-message (regex-replace "POSTER-SIZE" order-email-message poster-size))
    (setq order-email-message (regex-replace "POSTER-ORIENTATION" order-email-message poster-orientation))
    (setq order-email-message (regex-replace "ORDER-TOTAL" order-email-message order-total))
    (setq order-email-message (regex-replace "SHIPPING-ADDRESS" order-email-message address))
    (setq order-email-message (regex-replace "SHIPPING-CITY" order-email-message city))
    (setq order-email-message (regex-replace "SHIPPING-STATE" order-email-message state))
    (setq order-email-message (regex-replace "SHIPPING-ZIP" order-email-message zip))
    (setq order-email-message (regex-replace "DESTINATION-LINK" order-email-message destination-link))
    (email-mailgun-message *firefractal-from-email-address* (concatenate 'string "New Order #" order-id) order-email-message)
    (email-mailgun-message email (concatenate 'string "Order Receipt - firefractal.com - Order #" order-id) order-email-message)
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
         (order-id (jsown:val payment-gateway-response-json "id")))
    (if order-id
        (progn
          (send-firefractal-order-confirmation shipping-first-name shipping-last-name shipping-address shipping-city shipping-state shipping-zip shipping-email poster-size poster-orientation destination-link order-total order-id)
          "{\"success\":\"true\"}")
        "{\"success\":\"false\"}")
    ))