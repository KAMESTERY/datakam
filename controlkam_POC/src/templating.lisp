;;;; templating.lisp

;; Optimizations
(declaim (optimize (speed 3) (space 0) (safety 0) (debug 0)))

(in-package :controlkam)

(defun header (title)
  (eco-template:header title))

(defun footer (script)
  (eco-template:footer script))

(defun render (content &key (title "Control Kam") (script ""))
  (respond
   (eco-template:layout
    (header title)
    content
    (footer script))))

