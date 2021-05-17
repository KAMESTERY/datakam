(ns datakam.handlers.other.dispatch
  (:require [datakam.handlers.dispatch :refer [handle]]
            [datakam.handlers.other.endpoints :as ep]))

;; protected
(defmethod handle ::home [req-data]
  (ep/home (:req req-data)))

;; protected
(defmethod handle ::gql [req-data]
  (ep/gql (:req req-data)))
