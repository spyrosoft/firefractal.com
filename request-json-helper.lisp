;; This is all old, outdated and wrong

(defvar *firefractal-base-url* "http://firefractal.com/")
(defvar *full-path-to-fractal-images* #P"/home/firefractal/hunchentoot/static/images/fractals/")
(defvar *url-path-to-fractal-images* #P"images/fractals/")

(defun validate-json-value-array-of-arrays-length-three-of-integers (json-key json-value requirement)
	(let ((errors '())
			(minimum-value (second requirement))
			(maximum-value (third requirement)))
		(if (not (typep json-value 'list))
			(push (format nil "For ~S, ~S must be an array" json-key json-value) errors)
			(iterate (for rgb-list in json-value)
				(if (not (typep rgb-list 'list))
					(push (format nil "For ~S, ~S must be an array" json-key json-value) errors)
					(let ((r (first rgb-list))
							(g (second rgb-list))
							(b (third rgb-list)))
						(cond
							((or (not (typep r 'integer))
									(not (typep g 'integer))
									(not (typep b 'integer)))
								(push (format nil "For ~S, values of ~S must be integers." json-key json-value) errors))
							((or (> r maximum-value)
									(< r minimum-value)
									(> g maximum-value)
									(< g minimum-value)
									(> g maximum-value)
									(< g minimum-value))
								(push (format nil "For ~S, values of ~S must be between ~D and ~D inclusive." json-key json-value minimum-value maximum-value) errors)))))
			))
		errors
		))

(defun validate-json-value-string (json-key json-value requirement)
	(let ((errors '())
			(available-values (second requirement)))
		(when (not (member json-value available-values :test 'equal))
			(push (format nil "For ~S, ~S must be one of the following: \"~{~A~^\", \"~}" json-key json-value available-values) errors))
		errors
		))

(defun validate-json-value-number (json-key json-value requirement)
	(let ((errors '())
			(minimum-value (second requirement))
			(maximum-value (third requirement)))
		(when (and (< json-value 0) (< minimum-value json-value))
			(push (format nil "For ~S, ~D must be greater than ~D" json-key json-value minimum-value) errors))
		(if (and (>= json-value 0) (< json-value minimum-value))
			(push (format nil "For ~S, ~D must be greater than ~D" json-key json-value minimum-value) errors)
			(if (> json-value maximum-value)
				(push (format nil "For ~S, ~D must be less than ~D" json-key json-value maximum-value) errors)))
		errors
		))

(defun validate-json-value-by-type (json-key json-value requirement)
	(let ((errors '()) (requirement-type (first requirement)) (new-errors))
		(cond
			((or
					(eq requirement-type 'integer)
					(eq requirement-type 'float)
					(eq requirement-type 'real))
				(setq new-errors (validate-json-value-number json-key json-value requirement))
				(when new-errors
					(setq errors (concatenate 'list errors new-errors))))
			((eq requirement-type 'string)
				(setq new-errors (validate-json-value-string json-key json-value requirement))
				(when new-errors
					(setq errors (concatenate 'list errors new-errors))))
			((eq requirement-type 'array-of-arrays-length-three-of-integers)
				(setq new-errors (validate-json-value-array-of-arrays-length-three-of-integers json-key json-value requirement))
				(when new-errors
					(setq errors (concatenate 'list errors new-errors)))))
		errors
		))

(defun validate-json-value (json-key json-value requirement)
	(let ((errors '()) (requirement-type (first requirement)) (new-errors))
		(unless (typep json-value requirement-type)
			(push (format nil "For ~S, the value of ~S must be of type ~S." json-key json-value requirement-type) errors))
		(setq new-errors (validate-json-value-by-type json-key json-value requirement))
		(when new-errors
			(setq errors (concatenate 'list errors new-errors)))
		errors
		))

(defun validate-json-key (json-object json-key requirement)
	(let ((errors '()) (json-value))
		(handler-case (setq json-value (jsown:val json-object json-key))
			(error nil (push (concatenate 'string "The JSON key entitled " json-key " is required.") errors)))
		(unless errors
			(let ((validate-json-value-errors
					(validate-json-value json-key json-value requirement)))
				(when validate-json-value-errors
					(push validate-json-value-errors errors))
				))
		errors
		))

(defun convert-all-go-incompatible-json-values (json-object)
	(jsown:do-json-keys (json-object-key json-object-value)
		json-object
		(when (typep json-object-value 'ratio)
			(setf (jsown:val json-object json-object-key)
				(coerce json-object-value 'double-float))))
	json-object)

(defun get-required-json-keys (requirements)
	(loop for key being the hash-keys of requirements collect key))

(defun validate-json-string (json-string requirements)
	(let* ((errors '())
			(json-keys (get-required-json-keys requirements))
			(json-object))
		(handler-case (setq json-object (convert-all-go-incompatible-json-values
				(apply 'jsown:parse (append (list json-string) json-keys))))
			(error nil (push "Invalid JSON" errors)))
		(unless errors
			(iterate (for json-key in json-keys)
				(let ((json-key-error
						(validate-json-key
							json-object
							json-key
							(gethash json-key requirements))))
					(when json-key-error
						(setq errors (concatenate 'list errors (list json-key-error)))))))
		errors
		))

(defun get-final-destination-string (destination requirements email-address)
	(let* ((json-keys (get-required-json-keys requirements))
			(json-object (convert-all-go-incompatible-json-values
				(apply 'jsown:parse (append (list destination) json-keys))))
			(json-string-for-file-name-hash)
			(final-destination-string)
			(file-name)
			(full-path-to-file))
		(setq json-string-for-file-name-hash (jsown:to-json json-object))
		(setq json-string-for-file-name-hash (regex-replace-all "\\[\\]" json-string-for-file-name-hash "false"))
		(setq file-name (concatenate 'string (cryptos:md5 json-string-for-file-name-hash) ".png"))
		(setq full-path-to-file (concatenate 'string (namestring *full-path-to-fractal-images*) file-name))
		(jsown:extend-js json-object ("file-name" file-name))
		(jsown:extend-js json-object ("output-file" full-path-to-file))
		(jsown:extend-js json-object ("file-url" (concatenate 'string *firefractal-base-url* (namestring *url-path-to-fractal-images*) file-name)))
		(jsown:extend-js json-object ("email-address" email-address))
		(handler-case (jsown:extend-js json-object ("ip-address" (hunchentoot:remote-addr*)))
			(error nil nil))
		(setq final-destination-string (jsown:to-json json-object))
		(setq final-destination-string (regex-replace-all "\\[\\]" final-destination-string "false"))
		final-destination-string
		))