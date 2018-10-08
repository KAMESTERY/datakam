(in-package :controlkam)

(annot:enable-annot-syntax)

;; http://ergoemacs.org/emacs/elisp_basics.html
(defvar *keep-running* t)
@export
(defun wait()
  (tagbody
     (handler-bind ((condition
                      (lambda (c)
                        (declare (ignore c))
                        (go :escape))))
       (restart-bind ((my-continue
                        (lambda ()
                          (go :escape))))
         ;; (loop
         ;;    (sleep 1)
         ;;    (print :waiting)
         ;;    (terpri)
         ;;    (force-output))
         (loop :while *keep-running* :do
           (sleep 5))
         ))
   :escape
     (print :finished)))
