(ns datakam.core
  (:require-macros [fast-twitch.macros :as m])
  (:require [cljs.nodejs :as nodejs]
            [mount.core :as mount]
            [taoensso.timbre :as log]
            [fast-twitch.sugar :as ft]
            [fast-twitch.web-api :as web]
            [fast-twitch.server]
            [datakam.dispatch :refer [handle]]
            [datakam.routing :refer [routing-data]]
            ["morgan" :as logger]
            ["xhr2" :as xhr2]
            ["body-parser" :as body-parser]
            ["helmet" :as helmet]))

(nodejs/enable-util-print!)

(set! js/XMLHttpRequest xhr2)

(def routes
  (web/routes
    routing-data
    handle))

(defn middlewares []
  (let []
    [(helmet)
     (logger "combined")     ;; Logger
     (body-parser/json)      ;; support json encoded bodies
     (body-parser/urlencoded (clj->js {:extended true})) ;; support encoded bodies
     ]))

(defn main []
  (-> (mount/with-args
        {:ft {:middlewares (middlewares)
              :routes routes}})
      (mount/start)))

(set! *main-cli-fn* main)
