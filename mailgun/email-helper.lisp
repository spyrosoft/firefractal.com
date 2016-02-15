(defun email-mailgun-message-from-echo-at-firefractal (email-message-to-address email-message-subject email-message-body)
	(drakma:http-request
		*mailgun-email-post-url*
		:method :post
		:basic-authorization (list "api" *mailgun-api-key*)
		:parameters (list (cons "from" *firefractal-from-email-address*)
			(cons "to" email-message-to-address)
			(cons "bcc" *firefractal-from-email-address*)
			(cons "subject" email-message-subject)
			(cons "text" email-message-body))))

(defun email-mailgun-message (email-message-to-address email-message-subject email-message-body)
	(drakma:http-request
		*mailgun-email-post-url*
		:method :post
		:basic-authorization (list "api" *mailgun-api-key*)
		:parameters (list (cons "from" *firefractal-from-email-address*)
			(cons "to" email-message-to-address)
			(cons "subject" email-message-subject)
			(cons "text" email-message-body))))

(defun validate-email-address (email-address)
	(let* ((mailgun-response (map 'string #'code-char (drakma:http-request
		*mailgun-validate-email-get-url*
		:method :get
		:basic-authorization (list "api" *mailgun-api-public-key*)
		:parameters (list (cons "address" email-address)))))
			(mailgun-parsed-response)
			(response-is-valid nil)
			(errors '()))
		(handler-case (setq mailgun-parsed-response (jsown:parse mailgun-response))
			(error nil (push "Invalid JSON" errors)))
		(unless errors
			(handler-case (setq response-is-valid (jsown:val mailgun-parsed-response "is_valid"))
				(error nil (push "The JSON key 'is_valid' does not appear in the request response." errors))))
		(cond (errors
				(email-mailgun-message *dev-email-address* "firefractal.com Mailgun Validation Quit Working" (first errors))
				(list "Email validation has stopped functioning. The admin has been notified. This issue will be resolved as soon as possible."))
			(t
				(if response-is-valid
					'()
					(list "Invalid email address."))))
		))