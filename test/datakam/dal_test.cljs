(ns datakam.dal-test
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [contractskam.specs.thing-spec :as tspk]
            [datakam.dal :as dal]))

(defspec thing-create-retrieve-delete 1000
  (prop/for-all
   [thing (s/gen ::tspk/thing)]
   (let [thing-key (select-keys thing [:Name :ThingID])]
     (is (=
          (dal/put-thing thing)
          {}))
     (is (=
          (dal/get-thing thing-key)
          thing))
     (is (true?
          (empty? (dal/delete-thing thing-key))))
     ;; (if (and (s/valid? ::tspk/thing thing)
     ;;          (s/valid? ::tspk/thing-key thing-key))
     ;;   (do
     ;;     (is (=
     ;;          (dal/put-thing thing)
     ;;          {}))
     ;;     (is (=
     ;;          (dal/get-thing thing-key)
     ;;          thing))
     ;;     (is (true?
     ;;          (empty? (dal/delete-thing thing-key)))))
     ;;   (is (= 2 2)))
     )))

;; (run-tests)

;; (def thing {:UpdatedAt "2019-01-13 20:19:41.184138612 UTC",
;;             :UserID "hhhh@hhhh.hhh",
;;             :Score 0,
;;             :ThingID "com.kamestery.devdata:##:africa:##:project-kam-phoenix",
;;             :Version 0,
;;             :CreatedAt "2019-01-13 20:19:41.184138612 UTC",
;;             :Tags ["dev"],
;;             :Name "com.kamestery.devdata:##:africa"})

;; (def thing-key {:ThingID "com.kamestery.devdata:##:africa:##:project-kam-phoenix",
;;                 :Name "com.kamestery.devdata:##:africa"})

;; (deftest do-some-things
;;   (is (=
;;        (dal/put-thing thing)
;;        {}))
;;   (is (=
;;        (dal/get-thing thing-key)
;;        thing))
;;   (is (true?
;;        (empty? (dal/delete-thing thing-key)))))

