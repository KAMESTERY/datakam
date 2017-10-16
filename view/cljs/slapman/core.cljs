(ns slapman.core
  (:require [reagent.core :as reagent]
            [re-frame.core :as re-frame]
            [slapman.events]
            [slapman.subs]
            [slapman.routes :as routes]
            [slapman.views :as views]
            [slapman.config :as config]))


(defn dev-setup []
  (when config/debug?
    (enable-console-print!)
    (println "dev mode")))

(defn mount-root []
  (re-frame/clear-subscription-cache!)
  (reagent/render [views/main-panel]
                  (.getElementById js/document "app")))

(defn ^:export init []
  (routes/app-routes)
  (re-frame/dispatch-sync [:initialize-db])
  (dev-setup)
  (mount-root))
