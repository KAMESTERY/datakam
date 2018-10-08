#|
  This file is a part of controlkam project.
|#

(defsystem "controlkam"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("cl-autowrap"
               "dexador"
               "cl-json"
               "lucerne"
               "lparallel"
               "dyna"
               "zs3"
               "alexandria"
               "arrow-macros"
               "metabang-bind")
  :build-operation asdf:program-op
  :build-pathname "server"
  :entry-point "controlkam:main"
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "os")
                 (:file "controlkam"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "controlkam-test"))))
