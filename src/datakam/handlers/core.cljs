(ns datakam.handlers.core
  (:require [fast-twitch.web-api :as web]
            [datakam.handlers.home.dispatch]
            [datakam.handlers.home.routing :as dh]
            [datakam.handlers.other.dispatch]
            [datakam.handlers.other.routing :as do]
            [datakam.handlers.dispatch :refer [handle]]))

(def routing-data
  ["/"
   (conj
     dh/routing-data
     do/routing-data)
   ])

(def routes
  (web/routes
    routing-data
    handle))
