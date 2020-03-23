(ns datakam.gql.mutations)

(def mutations {:enroll
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
                 :resolve :delete_browse_media}})
