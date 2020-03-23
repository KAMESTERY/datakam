(ns datakam.gql.objects)

(def objects {:UnfurlOut
              {:fields
               {:url {:type 'String}
                :title {:type 'String}
                :description {:type 'String}
                :previewUrl {:type 'String}}}
              :User
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
               {:token {:type 'String}}}})
