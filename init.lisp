(ql:quickload :hunchentoot)
(ql:quickload :iterate)
(ql:quickload :jsown)
(ql:quickload :cl-ppcre)
(ql:quickload :crypto-shortcuts)
(ql:quickload :drakma)
(ql:quickload :flexi-streams)

(defpackage :firefractal
  (:use :common-lisp
        :hunchentoot
        :iterate
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
(load "credentials.lisp")
(load "mailgun/email-helper.lisp")
(load "routes.lisp")

(hunchentoot:start firefractal-server)