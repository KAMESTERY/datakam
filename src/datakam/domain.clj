(ns datakam.domain
  (:require [clojure.spec.alpha :as s]
            [clojure.set :refer [rename-keys]]
            [clojure.pprint :refer [pprint]]
            [datakam.specs.macros :refer [okspk?]]
            [datakam.specs.common-spec :as cspk]
            [datakam.specs.thing-spec :as tspk]
            [datakam.specs.data-spec :as dspk]
            [datakam.specs.document-spec :as docspk]
            [datakam.specs.media-spec :as mspk]
            [datakam.dal :as dal]
            [datakam.auth :as auth]))

;; QUERY

(defn query-media [m & options]
  {:pre  [(okspk? ::tspk/thing-like (tspk/thing-keys-localize m))]
   :post [(okspk? ::mspk/many-media-type (map mspk/media-keys-localize %))]}
  (let [things (apply dal/query-thing m options)]
    (pmap (fn [thing]
            (let [data (dal/query-data (select-keys thing [:ThingID]))]
              (mspk/thing-data-to-media thing data))) things)
    ))

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
                  (assoc :Media media)))) things)
    ))

;; GET

(defn get-document [dockey]
  {:pre  [(okspk? ::docspk/document-key (docspk/document-keys-localize dockey))]
   :post [(okspk? ::docspk/document (docspk/document-keys-localize %))]}
  (let [thing (dal/get-thing {:Name (:Topic dockey)
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
  (let [thing (dal/get-thing {:Name (:Topic mkey)
                              :ThingID (:MediaID mkey)})
        data (dal/query-data (select-keys thing [:ThingID]))]
    (mspk/thing-data-to-media thing data)))

(defn get-browse-media [mkey]
  {:pre  [(okspk? ::mspk/media-key (mspk/media-keys-localize mkey))]
   :post [(okspk? ::mspk/media-like (mspk/media-keys-localize %))]}
  (let [thing (dal/get-thing {:Name (:Type mkey)
                              :ThingID (:MediaID mkey)})
        data (dal/query-data (select-keys thing [:ThingID]))]
    (mspk/thing-data-to-media thing data)))

;; PUT (Private)

(defn- put-document [doc]
  {:pre  [(okspk? ::docspk/document (docspk/document-keys-localize doc))]
   :post [(okspk? empty? (:UnprocessedItems %))]}
  (let [doc-no-media (dissoc doc :Media)
        thing (docspk/doc-to-thing doc-no-media)
        data (docspk/doc-to-data doc-no-media)
        payload (atom (hash-map :Things {:Puts [thing]}
                                :Data {:Puts data}))]
    (doseq [m (:Media doc)]
        (let [t (mspk/media-to-thing m)
              a (mspk/media-to-association m)
              d (mspk/media-to-data m)]
             (swap! payload update-in [:Things :Puts] conj t)
             (swap! payload update-in [:Things :Puts] conj a)
             (swap! payload update-in [:Data :Puts] concat d)))
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

;; DELETE (Private)

(defn- delete-document [dockey]
  {:pre  [(okspk? ::docspk/document-key (docspk/document-keys-localize dockey))]
   :post [(okspk? empty? %)]}
  (let [thing (dal/get-thing {:Name (:Topic dockey)
                              :ThingID (:DocumentID dockey)})
        media (-> thing
                  (rename-keys {:ThingID :ParentDocumentID})
                  (select-keys [:ParentDocumentID])
                  query-media)
        media_associations (map #(mspk/media-to-thing %) media)
        data (dal/query-data (select-keys thing [:ThingID]))]
    (dal/batch-write (hash-map :Things {:Deletes (concat [thing] media_associations)}
                               :Data {:Deletes data}))
    ))

(defn- delete-media [mkey]
  {:pre  [(okspk? ::docspk/document-key (docspk/document-keys-localize mkey))]
   :post [(okspk? empty? %)]}
  (let [thing (dal/get-thing {:Name (:ParentDocumentID mkey)
                              :ThingID (:MediaID mkey)})]
    (dal/batch-write (hash-map :Things {:Deletes [thing]}))
    ))

(defn- delete-browse-media [mkey]
  {:pre  [(okspk? ::docspk/document-key (docspk/document-keys-localize mkey))]
   :post [(okspk? empty? %)]}
  (let [thing (dal/get-thing {:Name (:Type mkey)
                              :ThingID (:MediaID mkey)})
        data (dal/query-data (select-keys thing [:ThingID]))]
    (dal/batch-write (hash-map :Things {:Deletes [thing]}
                               :Data {:Deletes data}))
    ))

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
  (query-document {:Name "com.kamestery.devdata:##:africa"})
  )

