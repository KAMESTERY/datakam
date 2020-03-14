(ns datakam.gql.schema
  (:require [clojure.set :refer [rename-keys]]
            [com.walmartlabs.lacinia.schema :as schema]
            [com.walmartlabs.lacinia.util :as util]
            [unfurl.api :refer [unfurl]]
            [datakam.dal :as dal]
            [datakam.domain :as dmn]
            [datakam.auth :as auth]
            [datakam.gql.objects :refer [objects]]
            [datakam.gql.inputs :refer [inputs]]
            [datakam.gql.mutations :refer [mutations]]
            [datakam.gql.queries :refer [queries]]))

(def main-schema (-> {:objects objects
                      :input-objects inputs
                      :unions {:auth_types
                               {:members [:Msg :ErrorMsg :Token]}}
                      :mutations mutations
                      :queries queries}
                     (util/attach-resolvers {:hello (fn [_ _ _]
                                                      "world")
                                             :list_tables (fn [_ _ _]
                                                            (dal/list-tables))
                                             :un_furl (fn [ctx {:keys [unfurl_in]} v]
                                                        (-> unfurl_in
                                                            :url
                                                            unfurl
                                                            (rename-keys {:preview-url :previewUrl})))
                                             :get_user (fn [ctx {:keys [ukey]} v]
                                                         (dal/get-user ukey))
                                             :get_usergroup (fn [ctx {:keys [ugkey]} v]
                                                              (dal/get-usergroup ugkey))
                                             :put_userprofile (fn [ctx {:keys [token profile]} v]
                                                                (dmn/put-profile token profile))
                                             :get_userprofile (fn [ctx {:keys [upkey]} v]
                                                                (dal/get-userprofile upkey))
                                             :get_thing (fn [ctx {:keys [tkey]} v]
                                                          ;(println "Context: " ctx)
                                                          ;(println "Thing Key: " tkey)
                                                          ;(println "Value? " v)
                                                          (dal/get-thing tkey))
                                             :get_document (fn [ctx {:keys [dockey]} v]
                                                             (dmn/get-document dockey))
                                             :get_media (fn [ctx {:keys [mediabrowsekey]} v]
                                                          (dmn/get-media mediabrowsekey))
                                             :query_document (fn [ctx {:keys [query]} v]
                                                               (dmn/query-document query))
                                             :query_media (fn [ctx {:keys [query]} v]
                                                            (dmn/query-media query))
                                             :put_document (fn [ctx {:keys [token doc]} v]
                                                             (dmn/put-doc token doc))
                                             :put_media (fn [ctx {:keys [token med]} v]
                                                          (dmn/put-med token med))
                                             :delete_document (fn [ctx {:keys [token dockey]} v]
                                                                (dmn/delete-doc token dockey))
                                             :delete_media (fn [ctx {:keys [token mediakey]} v]
                                                             (dmn/delete-med token mediakey))
                                             :delete_browse_media (fn [ctx {:keys [token mediabrowsekey]} v]
                                                                    (dmn/delete-med token mediabrowsekey))
                                             :enroll (fn [ctx {:keys [creds]} v]
                                                       (let [res (auth/enroll! creds)]
                                                         (cond (contains? res :msg)
                                                               (schema/tag-with-type res :Msg)
                                                               (contains? res :errmsg)
                                                               (schema/tag-with-type res :ErrorMsg))))
                                             :authenticate (fn [ctx {:keys [creds]} v]
                                                             (let [res (auth/authenticate creds)]
                                                               (cond (contains? res :token)
                                                                     (schema/tag-with-type res :Token)
                                                                     (contains? res :errmsg)
                                                                     (schema/tag-with-type res :ErrorMsg))))})
                     schema/compile))
