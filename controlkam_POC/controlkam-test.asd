#|
  This file is a part of controlkam project.
|#

(defsystem "controlkam-test"
  :defsystem-depends-on ("prove-asdf")
  :author ""
  :license ""
  :depends-on ("controlkam"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "controlkam"))))
  :description "Test system for controlkam"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
