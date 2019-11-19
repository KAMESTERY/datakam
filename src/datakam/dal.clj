(ns datakam.dal
  (:require [clojure.core.async :as a]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.string :as cljstr]
            [clojure.walk :refer [stringify-keys]]
            [cognitect.aws.client.api :as aws]
            [cognitect.aws.client.api.async :as aws.async]
            [com.rpl.specter :as S]
            [clojure.spec.alpha :as s]
            [datakam.specs.macros :refer [okspk?]]
            [datakam.specs.common-spec :as cspk]
            [datakam.specs.thing-spec :as tspk]
            [datakam.specs.data-spec :as dspk]
            [datakam.specs.user-spec :as uspk]
            [datakam.specs.userprofile-spec :as upspk]
            [datakam.specs.usergroup-spec :as ugspk]))

;; BatchWriteItems

(s/def ::domain-like (s/or
                      :t ::tspk/thing :tk ::tspk/thing-key
                      :d ::dspk/data :dk ::dspk/data-key
                      :u ::uspk/user :uk ::uspk/user-key
                      :up ::upspk/userprofile :upk ::upspk/userprofile-key
                      :ug ::ugspk/usergroup :ugk ::ugspk/usergroup-key))
(s/def ::Puts (s/* ::domain-like))
(s/def ::Deletes (s/* ::domain-like))
(s/def ::domain-batch (s/keys :req [::Puts ::Deletes]))
(s/def ::batch-write-items (s/map-of keyword? ::domain-batch))

;;;; Helpers

(defn- to-value [mval]
  (let [[t _] (keys mval)
        [v _] (vals mval)]
    (condp = t
      :N (edn/read-string v)
      :NS (map edn/read-string v)
      v)))

(defn- into-map-value [m]
  (->>
   (map (fn [[k mval]]
          (let [v (to-value mval)]
            {k v}))
        (seq m))
   (into {})))

(defn- derive-key-cond-expr-and-namevals [logic op sffx m]
  {:kce (cljstr/join (str " " (name logic) " ")
                     (map
                      (fn [k]
                        (let [flat (str "#" (name k) " " (name op) " :" (name k) (name sffx))
                              wrap (str (name op) "(#" (name k) ", :" (name k) (name sffx) ")")]
                          (condp = op
                            :BEGINS_WITH wrap
                            :CONTAINS wrap
                            flat)))
                      (keys m)))
   :ean (into {}
              (map #(hash-map (str "#" (name %)) %) (keys m)))})

(defn- update-map [m f]
  (reduce-kv (fn [m k v]
               (assoc m k (f v))) {} m))

(defn keys-append-suffix [m sffx & {:keys [tfn]
                                    :or {tfn keyword}}]
  (zipmap
   (map
    #(tfn (str (name %) (name sffx))) (keys m))
   (vals m)))

(defn keys-prepend-prefix [m prfx & {:keys [tfn]
                                     :or {tfn keyword}}]
  (zipmap
   (map
    #(tfn (str (name prfx) (name %))) (keys m))
   (vals m)))

(def ddb (aws/client {:api :dynamodb})) ;; Create a client to talk to DynamoDB

(defn- put-item [table item]
  (aws/invoke ddb {:op      :PutItem
                   :request {:TableName table
                             :Item      item}}))

(defn- get-item [table key & attrs-to-get]
  (-> (aws/invoke ddb {:op      :GetItem
                       :request {:TableName       table
                                 :Key             key
                                 :AttributesToGet attrs-to-get}})
      :Item
      into-map-value))

(defn- delete-item [table key]
  (aws/invoke ddb {:op      :DeleteItem
                   :request {:TableName table
                             :Key       key}}))

;; (defmacro item-getter [name table]
;;   `(defn ~(symbol (str "get-" name)) [ikey & attrs]
;;      {:pre [(s/valid? ~(symbol (str "::" name "-key")) (~(symbol (str name "-keys-localize")) ikey))
;;             (s/valid? ::resource-attrs attrs)]
;;       :post [(s/valid? ~(symbol (str "::" name)) (~(symbol (str name "-keys-localize")) %))]}
;;      (-> (apply get-item
;;                 ~table
;;                 (~(symbol (str name "-to-attrvals")) ikey)
;;                 attrs)
;;          :Item
;;          into-map-value))) ;; Revisit this!!

;; BatchWriteItems

(defn- batch-write-items-to-attrvals [m]
  (let [to-attrvals (fn [lm]
                      (cond
                        (s/valid?
                         ::tspk/thing-like
                         (tspk/thing-keys-localize lm)) (tspk/thing-to-attrvals lm)
                        (s/valid?
                         ::dspk/data-like
                         (dspk/data-keys-localize lm)) (dspk/data-to-attrvals lm)
                        (s/valid?
                         ::uspk/user-like
                         (uspk/user-keys-localize lm)) (uspk/user-to-attrvals lm)
                        (s/valid?
                         ::upspk/userprofile-like
                         (upspk/userprofile-keys-localize lm)) (upspk/userprofile-to-attrvals lm)
                        (s/valid?
                         ::ugspk/usergroup-like
                         (ugspk/usergroup-keys-localize lm)) (ugspk/usergroup-to-attrvals lm)
                        :else lm))
                                        ;kvec (-> m keys (into []))
        resm (S/multi-transform [(S/multi-path
                                  :Things :Data :User :UserProfile :UserGroup)
                                 (S/multi-path :Puts :Deletes)
                                        ;(S/filterer not-empty)
                                 (S/terminal
                                  #(map
                                    to-attrvals %))] m)]
                                        ;(select-keys resm kvec)
    resm))

;; QueryItems

(defn- query [table m & {:keys [op logic prfx sffx fop fltr]
                         :or {op := logic :AND prfx ":" sffx :Q fop := fltr {}}}]
  (let [{kce :kce ean :ean} (derive-key-cond-expr-and-namevals
                             logic op sffx m)
        {fkce :kce fean :ean} (derive-key-cond-expr-and-namevals
                               logic fop sffx fltr)
        qean (merge ean fean)
        qdata (merge m fltr)
        eav (-> (condp = table
                  :Things (tspk/thing-to-attrvals qdata)
                  :Data (dspk/data-to-attrvals qdata)
                  :User (uspk/user-to-attrvals qdata)
                  :UserProfile (upspk/userprofile-to-attrvals qdata)
                  :UserGroup (ugspk/usergroup-to-attrvals qdata)
                  qdata)
                (keys-append-suffix sffx)
                (keys-prepend-prefix prfx :tfn str))
        req {:TableName                 table
             :KeyConditionExpression    kce
             :ExpressionAttributeNames qean
             :ExpressionAttributeValues eav}]
    (println "Query ExpressionAttributeNames: " qean)
    (println "Query Data: " qdata)
    (println "ExpressionAttributeValues: " eav)
    (println "KCE: " kce)
    (println "FKCE: " fkce)
    (map into-map-value
         (-> (aws/invoke ddb
                         {:op      :Query
                          :request (if (-> fkce cljstr/blank? not)
                                     (merge req {:FilterExpression fkce})
                                     req)})
             :Items))))


;;;; API


(defn list-tables []
  (-> (aws/invoke ddb {:op :ListTables})
      :TableNames))

;; (defn describe-tables [& tables]
;;   (->> tables
;;        (map #(aws/invoke ddb {:op      :DescribeTable
;;                               :request {:TableName %}
;;                               :ch      (a/promise-chan (comp
;;                                                         (map :Table)
;;                                                         (map :TableStatus)))}))
;;        (into #{})))

(defn batch-write [items]
  {:pre  [(s/valid? ::batch-write-items items)]
   :post [(s/valid? any? %)]}
  (let [req-data (batch-write-items-to-attrvals items)
        rits (map
              (fn [k]
                {k (concat
                    (map
                     (fn [lst] {:PutRequest {:Item lst}})
                     (-> req-data k :Puts))
                    (map
                     (fn [lst] {:DeleteRequest {:Key lst}})
                     (-> req-data k :Deletes)))})
              (keys req-data))
        req-items (into {}
                        (filter (comp not-empty val)
                                (into {} rits)))]
    (aws/invoke ddb {:op      :BatchWriteItem
                     :request {:RequestItems req-items}})))

;;;; ThingDB DSL

;; GET


(defn get-thing [tkey & attrs]
  {:pre  [(s/valid? ::tspk/thing-key (tspk/thing-keys-localize tkey))
          (s/valid? ::cspk/resource-attrs attrs)]
   :post [(s/valid? ::tspk/thing (tspk/thing-keys-localize %))]}
  (apply get-item
         "Things"
         (tspk/thing-to-attrvals tkey)
         attrs))


;; (item-getter "thing" "Things")

(defn get-data [dkey & attrs]
  {:pre  [(s/valid? ::dspk/data-key (dspk/data-keys-localize dkey))
          (s/valid? ::cspk/resource-attrs attrs)]
   :post [(s/valid? ::dspk/data (dspk/data-keys-localize %))]}
  (apply get-item
         "Data"
         (dspk/data-to-attrvals dkey)
         attrs))

(defn get-user [ukey & attrs]
  {:pre  [(s/valid? ::uspk/user-key (uspk/user-keys-localize ukey))
          (s/valid? ::cspk/resource-attrs attrs)]
   :post [(s/valid? ::uspk/user (uspk/user-keys-localize %))]}
  (apply get-item
         "User"
         (uspk/user-to-attrvals ukey)
         attrs))

(defn get-userprofile [upkey & attrs]
  {:pre  [(s/valid? ::upspk/userprofile-key (upspk/userprofile-keys-localize upkey))
          (s/valid? ::cspk/resource-attrs attrs)]
   :post [(s/valid? ::upspk/userprofile (upspk/userprofile-keys-localize %))]}
  (apply get-item
         "UserProfile"
         (upspk/userprofile-to-attrvals upkey)
         attrs))

(defn get-usergroup [ugkey & attrs]
  {:pre  [(s/valid? ::ugspk/usergroup-key (ugspk/usergroup-keys-localize ugkey))
          (s/valid? ::cspk/resource-attrs attrs)]
   :post [(s/valid? ::ugspk/usergroup (ugspk/usergroup-keys-localize %))]}
  (apply get-item
         "UserGroups"
         (ugspk/usergroup-to-attrvals ugkey)
         attrs))

;; PUT

(defn put-thing [thing]
  {:pre  [(s/valid? ::tspk/thing (tspk/thing-keys-localize thing))]
   :post [(s/valid? empty? %)]}
  (put-item
   :Things
   (tspk/thing-to-attrvals thing)))

(defn put-data [data]
  {:pre  [(s/valid? ::dspk/data (dspk/data-keys-localize data))]
   :post [(s/valid? empty? %)]}
  (put-item
   :Data
   (dspk/data-to-attrvals data)))

(defn put-user [user]
  {:pre  [(s/valid? ::uspk/user (uspk/user-keys-localize user))]
   :post [(s/valid? empty? %)]}
  (put-item
   :User
   (uspk/user-to-attrvals user)))

(defn put-userprofile [userprofile]
  {:pre  [(s/valid? ::upspk/userprofile (upspk/userprofile-keys-localize userprofile))]
   :post [(s/valid? empty? %)]}
  (put-item
   :UserProfile
   (upspk/userprofile-to-attrvals userprofile)))

(defn put-usergroup [usergroup]
  {:pre  [(s/valid? ::ugspk/usergroup (ugspk/usergroup-keys-localize usergroup))]
   :post [(s/valid? empty? %)]}
  (put-item
   :UserGroup
   (ugspk/usergroup-to-attrvals usergroup)))

;; DELETE

(defn delete-thing [thing]
  {:pre  [(s/valid? ::tspk/thing-key (tspk/thing-keys-localize thing))]
   :post [(s/valid? empty? %)]}
  (delete-item
   :Things
   (tspk/thing-to-attrvals thing)))

(defn delete-data [data]
  {:pre  [(s/valid? ::dspk/data-key (dspk/data-keys-localize data))]
   :post [(s/valid? empty? %)]}
  (delete-item
   :Data
   (dspk/data-to-attrvals data)))

(defn delete-user [user]
  {:pre  [(s/valid? ::uspk/user-key (uspk/user-keys-localize user))]
   :post [(s/valid? empty? %)]}
  (delete-item
   :User
   (uspk/user-to-attrvals user)))

(defn delete-userprofile [userprofile]
  {:pre  [(s/valid? ::upspk/userprofile-key (upspk/userprofile-keys-localize userprofile))]
   :post [(s/valid? empty? %)]}
  (delete-item
   :UserProfile
   (upspk/userprofile-to-attrvals userprofile)))

(defn delete-usergroup [usergroup]
  {:pre  [(s/valid? ::ugspk/usergroup-key (ugspk/usergroup-keys-localize usergroup))]
   :post [(s/valid? empty? %)]}
  (delete-item
   :UserGroup
   (ugspk/usergroup-to-attrvals usergroup)))

;; QUERY

(defn query-thing [m & options]
  {:pre [(s/valid? ::tspk/thing-like (tspk/thing-keys-localize m))]
   :post [(s/valid? ::tspk/many-things-type (map tspk/thing-keys-localize %))]}
  (apply query :Things m options))

(defn query-data [m & options]
  {:pre [(s/valid? ::dspk/data-like (dspk/data-keys-localize m))]
   :post [(s/valid? ::dspk/many-data-type (map dspk/data-keys-localize %))]}
  (apply query :Data m options))

(defn query-user [m & options]
  {:pre [(s/valid? ::uspk/user-like (uspk/user-keys-localize m))]
   :post [(s/valid? ::uspk/many-user-type (map uspk/user-keys-localize %))]}
  (apply query :User m options))

(defn query-userprofile [m & options]
  {:pre [(s/valid? ::upspk/userprofile-like (upspk/userprofile-keys-localize m))]
   :post [(s/valid? ::upspk/many-userprofile-type (map upspk/userprofile-keys-localize %))]}
  (apply query :UserProfile m options))

(defn query-usergroup [m & options]
  {:pre [(s/valid? ::ugspk/userproup-like (ugspk/usergroup-keys-localize m))]
   :post [(s/valid? ::ugspk/many-usergroup-type (map ugspk/usergroup-keys-localize %))]}
  (apply query :UserGroup m options))

;;; Protocols

(defprotocol Crud
  "CRUD Protocol"
  (get-record [this tkey & attrs] "Retrieve one Record")
  (put-record [this data] "Put one Record")
  (query-records [this m & options] "Query Records")
  (delete-record [this tkey] "Delete Record"))

;;;; Records

(defrecord Thing [Name
                  ThingID
                  UserID
                  Tags
                  Score
                  Version
                  CreatedAt
                  UpdatedAt]
  Crud
  (get-record [this tkey & attrs]
    (apply get-thing tkey attrs))
  (put-record [this data]
    (put-thing data))
  (query-records [this m & options]
    (apply query-thing m options))
  (delete-record [this tkey]
    (delete-thing tkey)))



;;;; Scratch Pad

;; ask what it can do
(comment
  (aws/ops ddb)

  ;; doc!
  (aws/doc ddb :ListTables)
  (aws/doc ddb :CreateTable)
  (aws/doc ddb :Scan) ;; this one has references in the request/response
  (aws/doc ddb :Query)

  (get-thing
   {:Name    "com.kamestery.devdata:##:africa"
    :ThingID "com.kamestery.devdata:##:africa:##:project-kam"})

  (query-thing
   {:Name    "com.kamestery.devdata:##:africa"})

  (query-thing
   {:Name    "com.kamestery.devdata:##:africa"
    :ThingID "com.kamestery.devdata:##:africa:##:project-kam"})

  (query-thing
   {:Name    "com.kamestery.devdata:##:africa"}
   :fop :BEGINS_WITH
   :fltr {:ThingID "com.kamestery.devdata:##:africa:##:project-"})

  (query-thing
   {:Name    "com.kamestery.devdata:##:africa"}
   :fop :CONTAINS
   :fltr {:Tags ["inspiration"]})

  (query-thing
   {:Name    "com.kamestery.devdata:##:some-bogus-topic"})

  (get-item :Things {:Name    {:S "com.kamestery.devdata:##:africa"}
                     :ThingID {:S "com.kamestery.devdata:##:africa:##:project-kam"}})
  (get-data
   {:DataID  "27ed56f9-05ec-4105-abfb-103b9d4a8854"
    :ThingID "com.kamestery.devdata:##:africa:##:project-kam"})

  (get-item :Data
            {:DataID  {:S "27ed56f9-05ec-4105-abfb-103b9d4a8854"}
             :ThingID {:S "com.kamestery.devdata:##:africa:##:project-kam"}}))

