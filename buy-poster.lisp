(defun new-firefractal-order (first-name last-name address city state zip email)
  (let ((order-email-message ""))
    
  ))

(define-easy-handler (buy-poster
                      :uri "/buy-poster/"
                      :default-request-type :post)
  (shipping-first-name shipping-last-name shipping-address shipping-city shipping-state shipping-zip shipping-email)
  (new-firefractal-order shipping-first-name shipping-last-name shipping-address shipping-city shipping-state shipping-zip shipping-email)
  (read-file-into-string "email-templates/order-received.txt")
  )


;; (defun send-feedback-email (name email message)
;; 	(let ((feedback-message ""))
;; 		(handler-case (setq feedback-message (concatenate 'string feedback-message (hunchentoot:remote-addr*) "

;; "))
;; 			(error nil nil))
;; 		(when email
;; 			(setq feedback-message (concatenate 'string feedback-message "From: " email "

;; ")))
;; 		(when name
;; 			(setq feedback-message (concatenate 'string feedback-message "Name: " name "

;; ")))
;; 		(when message
;; 			(setq feedback-message (concatenate 'string feedback-message message))
;; 			(email-mailgun-message *firefractal-from-email-address* "Feedback Form Submission - firefractal.com" feedback-message))))

;; (define-easy-handler (feedback-form
;; 		:uri "/feedback"
;; 		:default-request-type :post)
;; 	(name email message)
;; 	(send-feedback-email name email message)
;; 	"{\"success\":true}"
;; 	)