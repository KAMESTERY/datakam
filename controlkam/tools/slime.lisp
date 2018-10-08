;;; push current directory into asdf so quicklisp can load us
#+(or sbcl clisp) (push (first (directory "../")) asdf:*central-registry*)

;;; register local project in current directory
#+(or abcl ccl ecl) (setf ql:*local-project-directories* '(#p"../"))(ql:register-local-projects)

(format t "~a~%" (lisp-implementation-type))
(format t "~a~%" (lisp-implementation-version))
(format t "~a~%" *runtime-pathname*)

(ql:quickload :controlkam)
(ql:quickload :controlkam-test)

