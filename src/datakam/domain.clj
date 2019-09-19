(ns datakam.domain
  (:require [clojure.spec.alpha :as s]
            [datakam.specs.macros :refer [okspk?]]
            [datakam.specs.common-spec :as cspk]
            [datakam.specs.thing-spec :as tspk]
            [datakam.specs.data-spec :as dspk]
            [datakam.specs.document-spec :as docspk]
            [datakam.dal :as dal]))

;; GET

(defn get-document [dockey]
  {:pre  [(okspk? ::docspk/document-key (docspk/document-keys-localize dockey))]
   :post [(okspk? ::docspk/document (docspk/document-keys-localize %))]}
  (let [thing (dal/get-thing {:Name (:Topic dockey)
                              :ThingID (:DocumentID dockey)})
        data (dal/query-data (select-keys thing [:ThingID]))]
    (docspk/thing-data-to-document thing data)))

;; PUT

(defn put-document [doc]
  {:pre  [(okspk? ::docspk/document (docspk/document-keys-localize doc))]
   :post [(okspk? empty? %)]}
  (let [thing (docspk/doc-to-thing doc)
        data (docspk/doc-to-data doc)]
    (dal/batch-write (hash-map :Things {:Puts [thing]}
                               :Data {:Puts data}))))

;; QUERY

(defn query-document [m & options]
  {:pre  [(okspk? ::tspk/thing-like (tspk/thing-keys-localize m))]
   :post [(okspk? ::docspk/many-document-type (map docspk/document-keys-localize %))]}
  (let [things (apply dal/query-thing m options)]
    (pmap (fn [thing]
            (let [data (dal/query-data (select-keys thing [:ThingID]))]
              (docspk/thing-data-to-document thing data))) things)
    ))

;; DELETE

(defn delete-document [dockey]
  {:pre  [(okspk? ::docspk/document-key (docspk/document-keys-localize dockey))]
   :post [(okspk? empty? %)]}
  (let [thing (dal/get-thing {:Name (:Topic dockey)
                              :ThingID (:DocumentID dockey)})
        data (dal/query-data (select-keys thing [:ThingID]))]
    (dal/batch-write (hash-map :Things {:Deletes [thing]}
                               :Data {:Deletes data}))))

(comment
  (query-document {:Name "com.kamestery.devdata:##:africa"})
  )

