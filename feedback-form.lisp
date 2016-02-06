(defun send-feedback-email (name email message)
	(let ((feedback-message ""))
		(handler-case (setq feedback-message (concatenate 'string feedback-message (hunchentoot:remote-addr*) "

"))
			(error nil nil))
		(when email
			(setq feedback-message (concatenate 'string feedback-message "From: " email "

")))
		(when name
			(setq feedback-message (concatenate 'string feedback-message "Name: " name "

")))
		(when message
			(setq feedback-message (concatenate 'string feedback-message message))
			(email-mailgun-message *firefractal-from-email-address* "Feedback Form Submission - firefractal.com" feedback-message))))

(define-easy-handler (feedback-form
		:uri "/feedback"
		:default-request-type :post)
	(name email message)
	(send-feedback-email name email message)
	"{\"success\":true}"
	)