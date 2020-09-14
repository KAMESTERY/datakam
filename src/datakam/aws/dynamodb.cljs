(ns datakam.aws.dynamodb
  (:require [cljs.core.async :refer [go chan put! <!]]
            [cljs.core.async.interop :refer-macros [<p!]]
            [taoensso.timbre :as log]
            ["aws-sdk" :as aws]))

(aws/config.update #js {:region "us-east-1"})

(defn new-client [_]
  (aws/DynamoDB.DocumentClient.))

(defn put [ddbClient data]
  (go
    (try
      (let [res (<p! (.promise
                      (.put ddbClient (clj->js data))))]
        (js->clj res :keywordize-keys true))
      (catch js/Error err
        (log/error "ERROR:::: Unable to persist data: " (ex-cause err))))))

(defn batch-write [ddbClient data]
  (go
    (try
      (let [res (<p! (.promise
                      (.batchWriteItem ddbClient (clj->js data))))]
        (js->clj res :keywordize-keys true))
      (catch js/Error err
        (log/error "ERROR:::: Unable to batch persist data: " (ex-cause err))))))

;; (defn get [ddbClient data]
;;   (log/debug data)
;;   (.log js/console (clj->js data))
;;   (try
;;     (-> (.promise
;;          (.get ddbClient (clj->js data)))
;;         ;;(js/Promise.resolve)
;;         (.then (fn [res]
;;                  (log/debug (js->clj res :keywordize-keys true))
;;                  (js->clj % :keywordize-keys true))))
;;     (catch js/Error err
;;       (log/error "ERROR:::: Unable to retrieve data: " (ex-cause err)))))

(defn get [ddbClient data]
  ;; (log/debug data)
  ;; (.log js/console (clj->js data))
  (go
    (try
      (let [res (<p! (.promise
                      (.get ddbClient (clj->js data))))
            response (js->clj res :keywordize-keys true)]
        ; (.log js/console res)
        ; (.log js/console (str "Get Response: " res))
        (log/debug response)
        (-> response :Item))
      (catch js/Error err
        (log/error "ERROR:::: Unable to retrieve data: " (ex-cause err))))))

(defn batch-get [ddbClient data]
  (go
    (try
      (let [res (<p! (.. ddbClient batchGetItem (clj->js data)))]
        (js->clj res :keywordize-keys true))
      (catch js/Error err
        (log/error "ERROR:::: Unable to batch retrieve data: " (ex-cause err))))))

(defn update [ddbClient data]
  (go
    (try
      (let [res (<p! (.promise
                      (.update ddbClient (clj->js data))))]
        (js->clj res :keywordize-keys true))
      (catch js/Error err
        (log/error "ERROR:::: Unable to update data: " (ex-cause err))))))

(defn delete [ddbClient data]
  (go
    (try
      (let [res (<p! (.promise
                      (.delete ddbClient (clj->js data))))]
        (js->clj res :keywordize-keys true))
      (catch js/Error err
        (log/error "ERROR:::: Unable to delete data: " (ex-cause err))))))

(defn query [ddbClient data]
  (log/debug data)
  (.log js/console (clj->js data))
  (go
    (try
      (let [res (<p! (.promise
                      (.query ddbClient (clj->js data))))
            reponse (js->clj res :keywordize-keys true)]
        (log/debug response)
        response)
      (catch js/Error err
        (log/error "ERROR:::: Unable to execute query: " (ex-cause err))))))

(defn list-tables [ddbClient data]
  (go
    (try
      (let [res (<p! (.promise
                      (.listTables ddbClient (clj->js data))))]
        (js->clj res :keywordize-keys true))
      (catch js/Error err
        (log/error "ERROR:::: Unable to list tables: " (ex-cause err))))))

(defn scan [ddbClient data]
  (go
    (try
      (let [res (<p! (.promise
                      (.scan ddbClient (clj->js data))))]
        (js->clj res :keywordize-keys true))
      (catch js/Error err
        (log/error "ERROR:::: Unable to scan data: " (ex-cause err))))))

