(defpackage controlkam
  (:use :cl))
(in-package :controlkam)

;; blah blah blah.

(annot:enable-annot-syntax)

@export
(defapp app
  :middlewares (clack.middleware.accesslog:<clack-middleware-accesslog>
                (clack.middleware.backtrace:<clack-middleware-backtrace>)))

@route app "/"
(defview hello ()
  (respond "Hello, world!!"))

@export
(defun main()
  ;; Lisp Details
  (log:info "~a~%" (lisp-implementation-type))
  (log:info "~a~%" (lisp-implementation-version))

  ;; Logging
  (log:config :debug)
  ;;`'(log:config :info)
  (let* ((port (or (uiop:getenv "PORT")
                   "1881")))
    (start app :port (parse-integer port) :server :hunchentoot :debug t)
    (wait)))

