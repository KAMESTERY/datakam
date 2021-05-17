(ns datakam.handlers.home.endpoints
  (:require-macros [cljs.core.async.macros :refer [alt! go]]
                   [fast-twitch.macros :as m])
  (:require [cljs-http.client :as http]
            [cljs.core.async
             :as async
             :refer [<! put! chan close! timeout]]
            [clojure.pprint :refer [pprint]]
            [taoensso.timbre :as log]
            [com.rpl.specter :as s]
            [cljs.nodejs :as nodejs]
            [fast-twitch.os :as os]
            [fast-twitch.web-api :as web]
            [datakam.schema :as schema]))

(defn- on-timeout [default]
  "" " Control the Timeout from an Environment Variable " ""
  (let [t (if-let [TIMEOUT (m/env-var "TIMEOUT")] TIMEOUT default)]
    (timeout t)))

(defn home [req]
  (web/send "DataKam is Up!"))

(defn gql [req]
  (go
    (let [query (-> req :body :query)
          ;;query "{ authenticate(email: \"yzyz@yzyz.yz\", password: \"yzyzyzyz\") {token userId email role} }"
          ;;query "{ enroll(email: \"yzyz@yzyz.yz\", password: \"yzyzyzyz\") }"
          data-chan (chan 1)]
      (schema/gql-exec query #(put! data-chan (.parse js/JSON %)))
      (alt!
        data-chan
        ([data]
         (web/send :json
                   data
                   {:headers {:Content-Type "application/json"}
                    :status 200}))
        (on-timeout 2000)
        (web/send :json
                  {} ;; No Data
                  {:headers {:Content-Type "application/json"}
                   :status 200})))))
