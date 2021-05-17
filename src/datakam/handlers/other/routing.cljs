(ns datakam.handlers.other.routing
  (:require [datakam.handlers.other.dispatch :as od]))

(def routing-data
  ;; public
  {"other" {:get
            {"" ::od/home}}
   ;; protected
   "other/gql" {:post
                {"" ::od/gql}}})
