(ns datakam.gql.queries)

(def queries {:hello
              ;; String is quoted here; in EDN the quotation is not required
              {:type 'String
               :resolve :hello}
              :listtables
              {:type '(list String)
               :resolve :list_tables}
              :unfurl
              {:type :UnfurlOut
               :args {:unfurl_in {:type :UnfurlIn}}
               :resolve :un_furl}
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
               :resolve :authenticate}})
