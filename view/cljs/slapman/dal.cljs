(ns slapman.dal
  (:require [re-frame.core :as re-frame]
            [firebase-cljs.core :as f]
            [firebase-cljs.database :as fd]
            [firebase-cljs.database.query :as fdq]
            [firebase-cljs.database.reference :as fdr]
            [firebase-cljs.database.datasnapshot :as s]
            [firebase-cljs.auth :as fa]
            [firebase-cljs.auth.provider :as fap]
            [firebase-cljs.user :as u]
            [taoensso.timbre :as log]))

;; Options for Firebase App
(def fireOpts {:apiKey "AIzaSyDFt2FUB1ekiwpWJ-j0PnnMyO9Ar-zXq9A"
               :authDomain "slapman-c5b7b.firebaseapp.com"
               :databaseURL "https://slapman-c5b7b.firebaseio.com"
               :projectId "slapman-c5b7b"
               :storageBucket ""
               :messagingSenderId "239861454444"})

;; Initialize Firebase App
(defonce fireApp (f/init fireOpts))

;; Get Database for the Firebase App
(defonce fireDB (f/get-db fireApp))
