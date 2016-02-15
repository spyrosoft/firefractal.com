;; Add the following to init.lisp to use this:
;; (load "generate-png-worker.lisp")
;; (make-generate-png-thread)
;; (populate-generate-png-thread-with-cache)

(defvar *generate-png-queue* '())
(defvar *generate-png-queue-mutex* (sb-thread:make-mutex :name "Generate PNG queue lock"))
(defvar *generate-png-semaphore* (sb-thread:make-semaphore :name "Generate PNG semaphore" :count 0))
(defvar *generate-png-thread*)
(defvar *path-to-mandelbrot-png-generator* #P"/home/firefractal/hunchentoot/generate-mandelbrot-png")
(defvar *path-to-generate-png-queue-persistent-cache* #P"/home/firefractal/hunchentoot/generate-png-queue/")

(defun debug-to-file (message)
	(with-open-file (output-file-stream "/tmp/debug-output.txt"
			:direction :output
			:if-exists :append
			:if-does-not-exist :create)
		(format output-file-stream "~S~%" message)))

(defun email-user-success (destination-json-string)
	(let ((destination-json-object (jsown:parse destination-json-string)))
		(email-mailgun-message-from-echo-at-firefractal
			(jsown:val destination-json-object "email-address")
		  "Print Your Poster - firefractal.com"
			(concatenate 'string "Woooo!! Here's your link: "
				(jsown:val destination-json-object "file-url")))))

(defun email-failure (failure-message)
  (email-mailgun-message *firefractal-from-email-address* "Something Went Wrong - firefractal.com" failure-message))

(defun run-mandelbrot-png-generator (destination-json-string)
	(let ((output-string
			(with-input-from-string (input-stream destination-json-string)
				(with-output-to-string (output-stream)
					(sb-ext:run-program *path-to-mandelbrot-png-generator* '()
						:input input-stream
						:output output-stream)
				output-stream))))
		(if (equal output-string "success")
			(email-user-success destination-json-string)
			(email-failure (format nil "~S~%~%~S" destination-json-string output-string)))))

(defun generate-next-png-in-queue ()
	(let ((destination-png-to-generate) (destination-json-object))
		(sb-thread:with-mutex (*generate-png-queue-mutex*)
			(setq destination-png-to-generate (pop *generate-png-queue*)))
		(run-mandelbrot-png-generator destination-png-to-generate)
		(setq destination-json-object (jsown:parse destination-png-to-generate))
		(delete-file (concatenate 'string (namestring *path-to-generate-png-queue-persistent-cache*) (jsown:val destination-json-object "file-name") ".cache"))))

(defun make-generate-png-thread ()
	(setq *generate-png-thread* (sb-thread:make-thread (lambda ()
			(loop
				(sb-thread:wait-on-semaphore *generate-png-semaphore*)
				(generate-next-png-in-queue)))
			:name "Generate PNG Worker")))

(defun add-destination-to-persistent-cache (final-destination-string file-name)
	(with-open-file (file-stream (concatenate 'string (namestring *path-to-generate-png-queue-persistent-cache*) file-name ".cache")
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create)
		(format file-stream final-destination-string)))

(defun add-destination-to-png-queue (destination email-address)
	(let* ((final-destination-string (get-final-destination-string destination *destination-requirements* email-address))
			(destination-json-object (jsown:parse final-destination-string))
			(file-name (jsown:val destination-json-object "file-name")))
		(sb-thread:with-mutex (*generate-png-queue-mutex*)
			(setq *generate-png-queue* (append *generate-png-queue* (list final-destination-string)))
			(sb-thread:signal-semaphore *generate-png-semaphore*))
		(add-destination-to-persistent-cache final-destination-string file-name)
		))

(defun populate-generate-png-thread-with-cache ()
	(let ((files-with-invalid-json '()) (destination-json-object))
		(dolist (generate-png-cache-file (directory (concatenate 'string (namestring *path-to-generate-png-queue-persistent-cache*) "*.cache")))
			(let ((file-contents) (file-with-invalid-json))
				(with-open-file (file-stream generate-png-cache-file)
					(setq file-contents (make-string (file-length file-stream)))
					(read-sequence file-contents file-stream))
				(handler-case (setq destination-json-object (jsown:parse file-contents))
					(error nil (setq file-with-invalid-json generate-png-cache-file)))
				(when file-with-invalid-json
					(push file-with-invalid-json files-with-invalid-json))
				(unless file-with-invalid-json
					(add-destination-to-png-queue file-contents (jsown:val destination-json-object "email-address")))
				))
		(when files-with-invalid-json
			(email-mailgun-message *dev-email-address* "Invalid JSON on Queue Cache Read" (concatenate 'string "The following file(s) contain invalid JSON: " (format nil "~{~A, ~}" files-with-invalid-json))))
		))