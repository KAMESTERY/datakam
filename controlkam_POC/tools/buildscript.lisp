;;; push current directory into asdf so quicklisp can load us
#+(or sbcl clisp) (push (first (directory ".")) asdf:*central-registry*)

;;; register local project in current directory
#+(or abcl allegro ccl cmu ecl) (setf ql:*local-project-directories* '(#p"."))(ql:register-local-projects)

#+ecl (push (make-pathname :name nil :type nil :version nil
                           :defaults *load-truename*)
            asdf:*central-registry*)

;; Load Build Prerequisites
#+(or sbcl ccl clisp) (ql:quickload :eco)
#+(or sbcl ccl clisp) (ql:quickload :trivial-dump-core)

(format t "~a~%" (lisp-implementation-type))
(format t "~a~%" (lisp-implementation-version))

#+(or sbcl ccl) (ql:quickload :controlkam)
#+(or sbcl ccl) (in-package :controlkam)

;;; dump executable
;; #+sbcl (sb-ext:save-lisp-and-die #p"server"
;;                                  :toplevel 'main
;;                                  :executable t)

;; #+ccl (ccl:save-application #p"server"
;;                             :toplevel-function 'main
;;                             :prepend-kernel t)

#+ecl (progn
        (require 'asdf)
        (require 'cmp)

        (setf *load-verbose* nil)
        (setf *compile-verbose* nil)
        (setf c::*suppress-compiler-warnings* t)
        (setf c::*suppress-compiler-notes* t)

;;; This flag determines how lisp constants are compiled into the program.
;;; The default scheme does not work well in statically linked libraries
;;; yet.
        (setf c::*compile-in-constants* t)

        ;;(asdf:operate :build-op :controlkam :force t)

        (asdf:make-build  :controlkam
                          :type :program
                          ;;:type :static-library
                          :monolithic t
                          :epilogue-code '(ext:quit 0)
                          ;;:move-here "./"
                          ;;:move-here "./bin/"
                          )
        (quit))

#+(or abcl allegro cmu)(asdf:make-build  :controlkam :type :program
                                                   :monolithic t
                                                   :move-here "./bin/"
                                                   :epilogue-code '(ext:quit 0))

#+(or sbcl ccl clisp) (trivial-dump-core:save-executable #p"./server" #'main)

