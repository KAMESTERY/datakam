(ns datakam.handlers.home.routing
  (:require [datakam.handlers.home.dispatch :as hd]))

(def routing-data
  ;; public
  {"" {:get
       {"" ::hd/home}}
   ;; protected
   "gql" {:post
          {"" ::hd/gql}}})
