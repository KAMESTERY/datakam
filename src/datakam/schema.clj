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
            [datakam.domain :as dmn]
            [datakam.auth :as auth]))

(def main-schema (-> {:objects {:User
                                {:fields
                                 {:UserID {:type 'String}
                                  :Email {:type 'String}
                                  :Role {:type 'String}
                                  :Confirmed {:type 'Int}
                                  :Username {:type 'String}}}
                                :UserGroup
                                {:fields
                                 {:GroupID {:type 'String}
                                  :UserID {:type 'String}
                                  :Name {:type 'String}}}
                                :UserProfile
                                {:fields
                                 {:UserID {:type 'String}
                                  :AvatarHash {:type 'String}
                                  :AboutMe {:type 'String}
                                  :Name {:type 'String}
                                  :Age {:type 'Int}
                                  :Location {:type 'String}
                                  :MemberSince {:type 'String}}}
                                :Thing
                                {:fields
                                 {:Name {:type 'String}
                                  :ThingID {:type 'String}
                                  :UserID {:type 'String}
                                  :Tags {:type '(list String)}
                                  :Score {:type 'Int}
                                  :Version {:type 'Int}
                                  :CreatedAt {:type 'String}
                                  :UpdatedAt {:type 'String}}}
                                :MediaType
                                {:fields
                                 {:Type {:type 'String}
                                  :MediaID {:type 'String}
                                  :ParentDocumentID {:type 'String}
                                  :FileUrl {:type 'String}
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
                                  :Media {:type '(list :MediaType)}
                                  :Tags {:type '(list String)}
                                  :FiltreVisuel {:type 'Int}
                                  :Niveau {:type 'Int}
                                  :Langue {:type 'Int}
                                  :Score {:type 'Int}
                                  :Version {:type 'Int}
                                  :Title {:type 'String}
                                  :Body {:type 'String}
                                  :CreatedAt {:type 'String}
                                  :UpdatedAt {:type 'String}}}
                                :Msg
                                {:fields
                                 {:msg {:type 'String}}}
                                :ErrorMsg
                                {:fields
                                 {:errmsg {:type 'String}}}
                                :Token
                                {:fields
                                 {:token {:type 'String}}}}
                      :input-objects {:UserKey
                                      {:fields
                                       {:UserID {:type 'String}
                                        :Email {:type 'String}}}
                                      :UserGroupKey
                                      {:fields
                                       {:GroupID {:type 'String}
                                        :UserID {:type 'String}}}
                                      :UserProfileKey
                                      {:fields
                                       {:UserID {:type 'String}}}
                                      :InputUserProfile
                                      {:fields
                                       {:UserID {:type 'String}
                                        :AboutMe {:type 'String}
                                        :Name {:type 'String}
                                        :Age {:type 'Int}
                                        :Location {:type 'String}}}
                                      :ThingKey
                                      {:fields
                                       {:Name {:type 'String}
                                        :ThingID {:type 'String}}}
                                      :MediaKey
                                      {:fields
                                       {:ParentDocumentID {:type 'String}
                                        :MediaID {:type 'String}}}
                                      :MediaBrowseKey
                                      {:fields
                                       {:Type {:type 'String}
                                        :MediaID {:type 'String}}}
                                      :DocKey
                                      {:fields
                                       {:Topic {:type 'String}
                                        :DocumentID {:type 'String}}}
                                      :Query
                                      {:fields
                                       {:Name {:type 'String}}}
                                      :InputMediaType
                                      {:fields
                                       {:Type {:type 'String}
                                        :MediaID {:type 'String}
                                        :ParentDocumentID {:type 'String}
                                        :FileUrl {:type 'String}
                                        :UserID {:type 'String}
                                        :Tags {:type '(list String)}}}
                                      :InputDocument
                                      {:fields
                                       {:Topic {:type 'String}
                                        :DocumentID {:type 'String}
                                        :UserID {:type 'String}
                                        :Identifier {:type 'String}
                                        :Slug {:type 'String}
                                        :Publish {:type 'Boolean}
                                        :Media {:type '(list :InputMediaType)}
                                        :Tags {:type '(list String)}
                                        :FiltreVisuel {:type 'Int}
                                        :Langue {:type 'Int}
                                        :Niveau {:type 'Int}
                                        :Title {:type 'String}
                                        :Body {:type 'String}}}
                                      :Creds
                                      {:fields
                                       {:UserID {:type 'String}
                                        :Email {:type 'String}
                                        :Password {:type 'String}}}}
                      :unions {:auth_types
                               {:members [:Msg :ErrorMsg :Token]}}
                      :mutations {:enroll
                                  {:type :auth_types
                                   :args {:creds {:type :Creds}}
                                   :resolve :enroll}
                                  :putuserprofile
                                  {:type :Token
                                   :args {:token {:type 'String}
                                          :profile {:type :InputUserProfile}}
                                   :resolve :put_userprofile}
                                  :putdocument
                                  {:type :Token
                                   :args {:token {:type 'String}
                                          :doc {:type :InputDocument}}
                                   :resolve :put_document}
                                  :putmedia
                                  {:type :Token
                                   :args {:token {:type 'String}
                                          :doc {:type :InputMediaType}}
                                   :resolve :put_media}
                                  :deletedocument
                                  {:type :Token
                                   :args {:token {:type 'String}
                                          :dockey {:type :DocKey}}
                                   :resolve :delete_document}
                                  :deletefromdocument
                                  {:type :Token
                                   :args {:token {:type 'String}
                                          :mediakey {:type :MediaKey}}
                                   :resolve :delete_media}
                                  :deletefromcollection
                                  {:type :Token
                                   :args {:token {:type 'String}
                                          :mediabrowsekey {:type :MediaBrowseKey}}
                                   :resolve :delete_browse_media}}
                      :queries {:hello
                                ;; String is quoted here; in EDN the quotation is not required
                                {:type 'String
                                 :resolve :hello}
                                :listtables
                                {:type '(list String)
                                 :resolve :list_tables}
                                :getuser
                                {:type :User
                                 :args {:ukey {:type :UserKey}}
                                 :resolve :get_user}
                                :getusergroup
                                {:type :UserGroup
                                 :args {:ugkey {:type :UserGroupKey}}
                                 :resolve :get_usergroup}
                                :getuserprofile
                                {:type :UserProfile
                                 :args {:upkey {:type :UserProfileKey}}
                                 :resolve :get_userprofile}
                                :getthing
                                {:type :Thing
                                 :args {:tkey {:type :ThingKey}}
                                 :resolve :get_thing}
                                :getdocument
                                {:type :Document
                                 :args {:dockey {:type :DocKey}}
                                 :resolve :get_document}
                                :getmedia
                                {:type :MediaType
                                 :args {:mediabrowsekey {:type :MediaBrowseKey}}
                                 :resolve :get_media}
                                :querydocument
                                {:type '(list :Document)
                                 :args {:query {:type :Query}}
                                 :resolve :query_document}
                                :querymedia
                                {:type '(list :MediaType)
                                 :args {:query {:type :Query}}
                                 :resolve :query_media}
                                :authenticate
                                {:type :auth_types
                                 :args {:creds {:type :Creds}}
                                 :resolve :authenticate}}}
                     (util/attach-resolvers {:hello (fn [_ _ _]
                                                      "world")
                                             :list_tables (fn [_ _ _]
                                                            (dal/list-tables))
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
