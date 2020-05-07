(ns datakam.aws.core
  (:require [datakam.aws.dynamodb :as ddb]))

;;;; Client Instantiations

(defmulti client (fn [params] (:api params)))

(defmethod client :dynamodb [params]
  (ddb/new_client params))


;;;; DynamoDB Invocations

(defmulti invoke (fn [_ params] (:op params)))

(defmethod invoke :PutItem [client params]
  (ddb/put client (:request params)))

(defmethod invoke :BatchWritetItem [client params]
  (ddb/batch-write client (:request params)))

(defmethod invoke :GetItem [client params]
  (ddb/get client (:request params) (:ch params)))

(defmethod invoke :BatchGetItem [client params]
  (ddb/batch-get client (:request params)))

(defmethod invoke :DeleteItem [client params]
  (ddb/delete client (:request params)))

(defmethod invoke :Query [client params]
  (ddb/query client (:request params)))

(defmethod invoke :ListTables [client params]
  (ddb/list-tables client (:request params)))

(defmethod invoke :Scan [client params]
  (ddb/scan client (:request params)))

