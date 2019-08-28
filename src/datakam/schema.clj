;; (ns datakam.schema
;;   (:require [com.walmartlabs.lacinia.schema :as schema]))

;; (def main-schema (schema/compile
;;                   {:queries {:hello
;;                              ;; String is quoted here; in EDN the quotation is not required
;;                              {:type 'String
;;                               :resolve (constantly "world")}}}))


(ns datakam.schema
  (:require [com.walmartlabs.lacinia.schema :as schema]
            [com.walmartlabs.lacinia.util :as util]
            [datakam.dal :as dal]
            [datakam.domain :as dmn]))

(def main-schema (-> {:objects {:Thing
                                {:fields
                                 {:Name {:type 'String}
                                  :ThingID {:type 'String}
                                  :UserID {:type 'String}
                                  :Tags {:type '(list String)}
                                  :Score {:type 'Int}
                                  :Version {:type 'Int}
                                  :CreatedAt {:type 'String}
                                  :UpdatedAt {:type 'String}}}
                                :Document
                                {:fields
                                 {:Topic {:type 'String}
                                  :DocumentID {:type 'String}
                                  :UserID {:type 'String}
                                  :Identifier {:type 'String}
                                  :Slug {:type 'String}
                                  :Publish {:type 'Boolean}
                                  :Tags {:type '(list String)}
                                  :FiltreVisuel {:type 'Int}
                                  :Niveau {:type 'Int}
                                  :Score {:type 'Int}
                                  :Version {:type 'Int}
                                  :Body {:type 'String}
                                  :CreatedAt {:type 'String}
                                  :UpdatedAt {:type 'String}}}}
                      :input-objects {:ThingKey
                                      {:fields
                                        {:Name {:type 'String}
                                         :ThingID {:type 'String}}}
                                      :DocKey
                                       {:fields
                                        {:Topic {:type 'String}
                                         :DocumentID {:type 'String}}}
                                      :DocQuery
                                      {:fields
                                       {:Name {:type 'String}}}}
                      :queries {:hello
                                ;; String is quoted here; in EDN the quotation is not required
                                {:type 'String
                                 :resolve :hello}
                                :listtables
                                {:type '(list String)
                                 :resolve :list_tables}
                                :getthing
                                {:type :Thing
                                 :args {:tkey {:type :ThingKey}}
                                 :resolve :get_thing}
                                :getdocument
                                {:type :Document
                                 :args {:dockey {:type :DocKey}}
                                 :resolve :get_document}
                                :querydocument
                                 {:type '(list :Document)
                                  :args {:docquery {:type :DocQuery}}
                                  :resolve :query_document}}}
                     (util/attach-resolvers {:hello #(constantly "world")
                                             :list_tables dal/list-tables
                                             :get_thing (fn [ctx {:keys [tkey]} v]
                                                          (println "Context: " ctx)
                                                          (println "Thing Key: " tkey)
                                                          (println "Value? " v)
                                                          (dal/get-thing tkey))
                                             :get_document (fn [ctx {:keys [dockey]} v]
                                                             (dmn/get-document dockey))
                                             :query_document (fn [ctx {:keys [docquery]} v]
                                                               (dmn/query-document docquery))})
                     schema/compile))

