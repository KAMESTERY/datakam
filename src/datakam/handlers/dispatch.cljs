(ns datakam.handlers.dispatch
  (:require [fast-twitch.web-api :as web]))

(defmulti handle (fn [req-data] (:endpoint req-data)))

;; default
(defmethod handle :default [_]
  (web/send "Not Found"))
