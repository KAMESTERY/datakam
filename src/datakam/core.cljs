(ns datakam.core
  (:require-macros [fast-twitch.macros :as m])
  (:require [cljs.nodejs :as nodejs]
            [taoensso.timbre :as log]
            [fast-twitch.sugar :as ex]
            [fast-twitch.web-api :as web]
            [datakam.endpoints :as ep]
            [datakam.routing :refer [routing-data]]
            ["morgan" :as logger]
            ["xhr2" :as xhr2]
            ["body-parser" :as body-parser]
            ["helmet" :as helmet]))

(nodejs/enable-util-print!)

(set! js/XMLHttpRequest xhr2)

(defmulti handle (fn [req-data] (:endpoint req-data)))

;; protected
(defmethod handle :home [req-data]
  (ep/home (:req req-data)))

;; protected
(defmethod handle :gql [req-data]
  (ep/gql (:req req-data)))

;; default
(defmethod handle :default [_]
  (web/send "Not Found"))

(def routes
  (web/routes
    routing-data
    handle))

(defn main []
      (let [portNumber (if-let [PORT (m/env-var "PORT")] PORT 1818)]
           (log/debug "Port Number: " portNumber)
           (-> (ex/app)
               (ex/with-middleware (helmet))
               (ex/with-middleware (logger "combined"))     ;; Logger
               (ex/with-middleware (body-parser/json))      ;; support json encoded bodies
               (ex/with-middleware (body-parser/urlencoded (clj->js {:extended true}))) ;; support encoded bodies
               (ex/with-middleware "/" routes)
               (ex/listen portNumber))))

(set! *main-cli-fn* main)
