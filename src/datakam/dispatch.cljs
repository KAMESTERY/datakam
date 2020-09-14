(ns datakam.dispatch
  (:require [datakam.endpoints :as ep]
            [fast-twitch.web-api :as web]))

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
