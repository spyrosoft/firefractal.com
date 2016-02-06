(deftype array-of-arrays-length-three-of-integers ()
	`cl:list)

(defvar *test-destination*)
(setq *test-destination* "{\"zoom-level\": 60,
\"x\": -0.75,
\"y\": 0,
\"canvas-width\": 1600,
\"canvas-height\": 880,
\"max-iterations\": 100,
\"poster-size\": \"debug\",
\"invert-enabled\": nil,
\"color-sliders\": [[0,36,92],[255,0,0],[255,144,0],[4,160,63]]
}")

(define-easy-handler (print-poster
		:uri "/print-poster"
		:default-request-type :post)
	(destination email-address)
	(let ((errors (validate-json-string destination *destination-requirements*)))
		(concatenate 'list errors (validate-email-address email-address))
		(unless errors
			(add-destination-to-png-queue destination email-address))
		(if errors
			(concatenate 'string "{\"success\":false,\"errors\":\"" (format nil "~{~A~^ ~}" errors) "\"")
			(format nil "{\"success\":true,\"position-in-line\":~D}" (sb-thread:semaphore-count *generate-png-semaphore*)))
		))