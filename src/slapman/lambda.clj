(ns slapman.lambda
  (:gen-class
   :implements [com.amazonaws.services.lambda.runtime.RequestStreamHandler])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as s]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clj-http.client :as http]))

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
  (let [
        weather (-> (http/get
                     ;; "http://api.openweathermap.org/data/2.5/weather?q=~a&APPID=2614bbda39902b0d3c456a9df1792a3f"
                     "http://samples.openweathermap.org/data/2.5/weather?id=2172797&appid=b1b15e88fa797225412429c1c50c122a1")
                    :body
                    (json/read-str :key-fn key->keyword)
                    :main)
        ip (-> (http/get "https://api.ipify.org/?format=json")
               :body
               (json/read-str :key-fn key->keyword)
               :ip)
        data {:data
              {:msg "You have been Officially Slapped!!:-)"
               :ip ip
               :weather weather
               }}]
    (respond os data)))

(defn -handleRequest [this is os context]
  (let [event (parse-stream is)]
    (pprint context)
    (hello os event)))

