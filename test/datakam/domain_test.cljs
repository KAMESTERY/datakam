(ns datakam.domain-test
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [contractskam.specs.document-spec :as docspk]
            [datakam.domain :as dmn]))

(defspec document-create-retrieve-delete 1000
  (prop/for-all
   [doc (s/gen ::docspk/document)]
   (let [doc-key (select-keys doc [:Topic :DocumentID])]
     (is (=
          (#'dmn/put-document doc)
          {}))
     (is (=
          (dmn/get-document doc-key)
          doc))
     (is (true?
          (empty? (#'dmn/delete-document doc-key)))))))

;;;;;;;; About invoking private methods for testing
;; https://groups.google.com/forum/#!msg/clojure/Mi277rszUs0/G9Q0ua9nM1EJ
;; https://groups.google.com/forum/#!msg/clojure/mJqplAdt3TY/GjcWuXQzPKcJ
;; (#'some-ns/some-private-var some args) => to invoke private methods
;; (@#'some-ns/some-private-var some args) => to access private value

;; (run-tests)

