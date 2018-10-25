;;; push current directory into asdf so quicklisp can load us
#+(or sbcl clisp) (push (first (directory ".")) asdf:*central-registry*)

;;; register local project in current directory
#+(or abcl allegro ccl cmu ecl) (setf ql:*local-project-directories* '(#p"."))(ql:register-local-projects)

#+ecl (push (make-pathname :name nil :type nil :version nil
                           :defaults *load-truename*)
            asdf:*central-registry*)

(ql:quickload :controlkam-test)
(in-package :controlkam-test)

;;(asdf:test-system :controlkam)
(asdf:test-system :controlkam-test)

;; Same as 'asdf:test-system' except it returns T or NIL as the result of tests.
(prove:run :controlkam-test)

