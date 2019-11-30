(ns datakam.gql.inputs)

(def inputs {:UnfurlIn
             {:fields
              {:url {:type 'String}}}
             :UserKey
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
               :Password {:type 'String}}}})
