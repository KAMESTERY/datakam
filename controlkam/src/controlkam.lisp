;;;; controlkam.lisp

;; Optimizations
(declaim (optimize (speed 3) (space 0) (safety 0) (debug 0)))

(in-package :controlkam)

;; blah blah blah.

(annot:enable-annot-syntax)

@export
(defapp app
  :middlewares (clack.middleware.accesslog:<clack-middleware-accesslog>
                (clack.middleware.backtrace:<clack-middleware-backtrace>)
                (clack.middleware.static:<clack-middleware-static>
                 :path "/static/"
                 :root (asdf:system-relative-pathname :controlkam #p"static/"))))

@route app "/hello"
(defview hello ()
  (log:debug "Rendering Hello")
  (render "<h3>Hello from Multi Genius!! :-) :-)</h3>"))

@route app "/"
(defview home ()
  (log:debug "Rendering Home")
  (render
   (eco-template:home "Multi Genius")
   :title "Sweet Home"))

@export
(defun run (port &key (debug t))
  ;; Logging
  (if debug
      (log:config :debug)
      (log:config :info))
  ;; Lisp Details
  (log:info "~a~%" (lisp-implementation-type))
  (log:info "~a~%" (lisp-implementation-version))
  (start app :port (parse-integer port) :server :hunchentoot :debug debug))

@export
(defun main()
  (let* ((port (or (uiop:getenv "PORT")
                   "1881"))
         (debug (or (uiop:getenv "DEBUG")
                    nil)))
    (run port :debug debug)
    (wait)))

