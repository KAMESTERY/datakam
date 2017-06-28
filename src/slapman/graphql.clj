(ns slapman.graphql
  (:require [clojure.pprint :refer [pprint]]
            [com.walmartlabs.lacinia.schema :as schema]
            [com.walmartlabs.lacinia.pedestal :as lacinia]))

(def hello-schema (schema/compile
                   {:queries {:hello
                              ;; String is quoted here; in EDN the quotation is not required
                              {:type 'String
                               :resolve (constantly "world")}
                              :stuff
                              {:type 'String
                               :resolve (constantly "Good Stuff")}}}))

(def service (lacinia/pedestal-service hello-schema {:graphiql true}))

(pprint service)

