(ql:quickload :hunchentoot)
(ql:quickload :iterate)
(ql:quickload :jsown)
(ql:quickload :cl-ppcre)
(ql:quickload :crypto-shortcuts)
(ql:quickload :drakma)

(defpackage :firefractal
  (:use :common-lisp
        :hunchentoot
        :iterate
        :cl-ppcre
        :crypto-shortcuts))

(in-package :firefractal)

(setf hunchentoot::*show-lisp-errors-p* t)

(setq *read-default-float-format* 'double-float)

(defvar firefractal-server
  (make-instance 'hunchentoot:easy-acceptor
                 :document-root "static"
                 :error-template-directory "static/error-templates/"
                 :access-log-destination "logs/access.log"
                 :message-log-destination "logs/error.log"
                 :port 8082))

(load "utility-belt.lisp")
(load "destination-requirements.lisp")
(load "mailgun/email-helper.lisp")
(load "request-json-helper.lisp")
(load "generate-png-worker.lisp")
(make-generate-png-thread)
(populate-generate-png-thread-with-cache)
(load "routes.lisp")
(load "print-poster.lisp")
(load "feedback-form.lisp")

(hunchentoot:start firefractal-server)