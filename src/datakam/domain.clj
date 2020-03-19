(ns datakam.domain
  (:require [clojure.spec.alpha :as s]
            [clojure.set :refer [rename-keys]]
            [clojure.pprint :refer [pprint]]
            [taoensso.timbre :as log]
            [contractskam.specs.macros :refer [okspk?]]
            [contractskam.specs.common-spec :as cspk]
            [contractskam.specs.thing-spec :as tspk]
            [contractskam.specs.data-spec :as dspk]
            [contractskam.specs.document-spec :as docspk]
            [contractskam.specs.media-spec :as mspk]
            [datakam.dal :as dal]
            [datakam.auth :as auth]))

;; EXIST

(defn- thing-exists? [tkey]
  {:pre  [(s/valid? ::tspk/thing-like (tspk/thing-keys-localize tkey))]
   :post [(s/valid? boolean? %)]}
  (try
    (some? (dal/get-thing tkey))
    (catch java.lang.AssertionError e
      false)))

;; QUERY

(defn query-media [m & options]
  {:pre  [(okspk? ::tspk/thing-like (tspk/thing-keys-localize m))]
   :post [(okspk? ::mspk/many-media-type (map mspk/media-keys-localize %))]}
  (let [things (apply dal/query-thing m options)]
    (pmap (fn [thing]
            (let [data (dal/query-data (select-keys thing [:ThingID]))]
              (mspk/thing-data-to-media thing data))) things)))

(defn query-document [m & options]
  {:pre  [(okspk? ::tspk/thing-like (tspk/thing-keys-localize m))]
   :post [(okspk? ::docspk/many-document-type (map docspk/document-keys-localize %))]}
  (let [things (apply dal/query-thing m options)]
    (pmap (fn [thing]
            (let [data (dal/query-data (select-keys thing [:ThingID]))
                  media (-> thing
                            (select-keys [:ThingID])
                            (rename-keys {:ThingID :Name})
                            query-media)]
              (-> (docspk/thing-data-to-document thing data)
                  (assoc :Media media)))) things)))

;; GET


(defn get-document [dockey]
  {:pre  [(okspk? ::docspk/document-key (docspk/document-keys-localize dockey))]
   :post [(okspk? ::docspk/document (docspk/document-keys-localize %))]}
  (let [thing (dal/get-thing {:Name    (:Topic dockey)
                              :ThingID (:DocumentID dockey)})
        data (dal/query-data (select-keys thing [:ThingID]))
        media (-> thing
                  (select-keys [:ThingID])
                  (rename-keys {:ThingID :Name})
                  query-media)]
    (-> (docspk/thing-data-to-document thing data)
        (assoc :Media media))))

(defn get-media [mkey]
  {:pre  [(okspk? ::mspk/media-key (mspk/media-keys-localize mkey))]
   :post [(okspk? ::mspk/media-like (mspk/media-keys-localize %))]}
  (let [thing (dal/get-thing {:Name    (:Topic mkey)
                              :ThingID (:MediaID mkey)})
        data (dal/query-data (select-keys thing [:ThingID]))]
    (mspk/thing-data-to-media thing data)))

(defn get-browse-media [mkey]
  {:pre  [(okspk? ::mspk/media-key (mspk/media-keys-localize mkey))]
   :post [(okspk? ::mspk/media-like (mspk/media-keys-localize %))]}
  (let [thing (dal/get-thing {:Name    (:Type mkey)
                              :ThingID (:MediaID mkey)})
        data (dal/query-data (select-keys thing [:ThingID]))]
    (mspk/thing-data-to-media thing data)))

;; DELETE (Private)

(defn- delete-document [dockey]
  {:pre  [(okspk? ::docspk/document-key (docspk/document-keys-localize dockey))]
   :post [(okspk? empty? (:UnprocessedItems %))]}
  (if (thing-exists? {:Name    (:Topic dockey)
                      :ThingID (:DocumentID dockey)})
    (let [thing {:Name    (:Topic dockey)
                 :ThingID (:DocumentID dockey)}
          media (-> thing
                    (rename-keys {:ThingID :Name})
                    (select-keys [:Name])
                    query-media)
          media_associations (map #(-> % mspk/media-to-association (select-keys [:Name :ThingID])) media)
          data (map #(select-keys % [:DataID :ThingID]) (dal/query-data (select-keys thing [:ThingID])))
          payload (hash-map :Things {:Deletes (concat [thing] media_associations)}
                            :Data {:Deletes data})]
      (log/debug "Delete Payload: ")
      (pprint payload)
      (dal/batch-write payload))
    {:UnprocessedItems []}))

(defn- delete-media [mkey]
  {:pre  [(okspk? ::docspk/document-key (docspk/document-keys-localize mkey))]
   :post [(okspk? empty? %)]}
  (if (thing-exists? {:Name    (:ParentDocumentID mkey)
                      :ThingID (:MediaID mkey)})
    (let [thing {:Name    (:ParentDocumentID mkey)
                 :ThingID (:MediaID mkey)}]
      (dal/batch-write (hash-map :Things {:Deletes [thing]})))
    {:UnprocessedItems []}))

(defn- delete-browse-media [mkey]
  {:pre  [(okspk? ::docspk/document-key (docspk/document-keys-localize mkey))]
   :post [(okspk? empty? %)]}
  (if (thing-exists? {:Name    (:Type mkey)
                      :ThingID (:MediaID mkey)})
    (let [thing {:Name    (:Type mkey)
                 :ThingID (:MediaID mkey)}
          data (map #(select-keys % [:DataID :ThingID]) (dal/query-data (select-keys thing [:ThingID])))]
      (dal/batch-write (hash-map :Things {:Deletes [thing]}
                                 :Data {:Deletes data})))
    {:UnprocessedItems []}))

;; PUT (Private)

(defn- put-document [doc]
  {:pre  [(okspk? ::docspk/document (docspk/document-keys-localize doc))]
   :post [(okspk? empty? (:UnprocessedItems %))]}
  (when (thing-exists? {:Name    (:Topic doc)
                        :ThingID (:DocumentID doc)})
    (-> doc (select-keys [:Topic :DocumentID]) delete-document))
  (let [doc-no-media (dissoc doc :Media)
        thing (docspk/doc-to-thing doc-no-media)
        data (docspk/doc-to-data doc-no-media)
        payload (atom (hash-map :Things {:Puts [thing]}
                                :Data {:Puts data}))]
    (doseq [m (:Media doc)]
      (let [t (mspk/media-to-thing m)
            d (mspk/media-to-data m)
            a (mspk/media-to-association m)]
        (when-not (thing-exists? {:Name    (:Type m)
                                  :ThingID (:MediaID m)})
          (swap! payload update-in [:Things :Puts] conj t)
          (swap! payload update-in [:Data :Puts] concat d))
        (when-not (thing-exists? {:Name    (:ParentDocumentID m)
                                  :ThingID (:MediaID m)})
          (swap! payload update-in [:Things :Puts] conj a))))
    (swap! payload update-in [:Data :Puts] #(into [] %))
    (dal/batch-write @payload)))

(defn- put-media [media]
  {:pre  [(okspk? ::mspk/media (mspk/media-keys-localize media))]
   :post [(okspk? empty? %)]}
  (let [thing (mspk/media-to-thing media)
        association (mspk/media-to-association media)
        data (mspk/media-to-data media)]
    (dal/batch-write (hash-map :Things {:Puts [thing association]}
                               :Data {:Puts data}))))

;;;; REQUIREs AUTH

;; PUT

(defn put-doc [token doc]
  (let [new-token (auth/jwt-refresh token)]
    (put-document doc)
    {:token new-token}))

(defn put-med [token med]
  (let [new-token (auth/jwt-refresh token)]
    (put-media med)
    {:token new-token}))

(defn put-profile [token profile]
  (let [new-token (auth/jwt-refresh token)]
    (dal/put-userprofile profile)
    {:token new-token}))

;; DELETE

(defn delete-doc [token dockey]
  (let [new-token (auth/jwt-refresh token)]
    (delete-document dockey)
    {:token new-token}))

(defn delete-med [token mkey]
  (let [new-token (auth/jwt-refresh token)]
    (delete-media mkey)
    {:token new-token}))

(comment
  (query-document {:Name "com.kamestery.devdata:##:africa"}))

