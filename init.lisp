(ql:quickload :hunchentoot)
(ql:quickload :jsown)
(ql:quickload :cl-ppcre)
(ql:quickload :drakma)
(ql:quickload :flexi-streams)

(defpackage :firefractal
  (:use :common-lisp :hunchentoot))

(in-package :firefractal)

(setf hunchentoot::*show-lisp-errors-p* t)

(setq *read-default-float-format* 'double-float)

(defvar firefractal-server
  (make-instance 'hunchentoot:easy-acceptor
                 :document-root "web-root"
                 :error-template-directory "web-root/error-templates/"
                 :access-log-destination "logs/access.log"
                 :message-log-destination "logs/error.log"
                 :port 8082))

(load "utilities/utility-belt.lisp")
(load "packages/mailgun.lisp")
(load "private/credentials.lisp")
(load "routes.lisp")

(hunchentoot:start firefractal-server)