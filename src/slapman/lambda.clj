(ns slapman.lambda
  (:gen-class
   :implements [com.amazonaws.services.lambda.runtime.RequestStreamHandler])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as s]
            [clojure.data.json :as json]
            [clojure.java.io :as io]))

(defn- key->keyword [key-string]
  (-> key-string
      (s/replace #"([a-z])([A-Z])" "$1-$2")
      (s/replace #"([A-Z]+)([A-Z])" "$1-$2")
      (s/lower-case)
      (keyword)))

(defn- parse-stream [is]
  (let [data (json/read
              (io/reader is)
              :key-fn key->keyword)]
    (pprint data)
    data))

(defn- respond [os data]
  (let [w (io/writer os)
        r (or data {:data "Nada"})]
    (json/write r w)
    (.flush w)
    ))

(defn hello [os event]
  (respond os {:data {:msg "You have been Officially Slapped!!!!"}}))

(defn -handleRequest [this is os context]
  (let [event (parse-stream is)]
    (pprint context)
    (hello os event)))

