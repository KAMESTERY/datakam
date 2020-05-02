(ns datakam.schema
  (:require [taoensso.timbre :as log]
            [datakam.domain]
            ["graphql" :as gqlmod]))

(def graphql (aget gqlmod "graphql"))
(def build-schema (aget gqlmod "buildSchema"))

(def schema
  (build-schema
    "

      type UserAuthData {
        Token: String!
        UserID: String!
        Email: String!
        Role: Int!
      }

      type UserGroup {
        GroupId: String!
        GroupName: String!
      }

      type UserInfo {
        UserID: String!
        Email: String!
        Username: String!
        Role: String!
        Confirmed: String!
        LastSeen: String!
        Groups: [UserGroup!]
        AvatarHash: String!
        Name: String!
        Age: String!
        AboutMe: String!
        Location: String!
        MemberSince: String!
      }

      input InputMediaType {
        Type: String!,
        MediaID: String!,
        ParentDocumentID: String!,
        FileUrl: String!,
        UserID: String!,
        Tags: [String!]
      }

      type MediaType {
          Type: String!,
          MediaID: String,
          ParentDocumentID: String,
          FileUrl: String,
          UserID: String,
          Tags: [String],
          Score: Int,
          Version: Int,
          CreatedAt: String,
          UpdatedAt: String
      }

      input InputDocument {
          Topic: String!,
          DocumentID: String!,
          UserID: String!,
          Identifier: String!,
          Slug: String!,
          Publish: Boolean!,
          Tags: [String!]
          FiltreVisuel: Int!,
          Niveau: Int!,
          Langue: Int!,
          Title: String!,
          Body: String!
      }

      type Document {
          Topic: String!,
          DocumentID: String!,
          UserID: String!,
          Identifier: String!,
          Slug: String!,
          Publish: String!,
          Media: [MediaType!],
          Tags: [String!],
          FiltreVisuel: Int!,
          Niveau: Int!,
          Langue: Int!,
          Score: Int!,
          Version: Int!,
          Title: String!,
          Body: String!,
          CreatedAt: String!,
          UpdatedAt: String!,
          Related: [Document]
      }

      type DocumentsByTopic {
          Topic: String!,
          Docs: [Document!]
      }

      type Mutation {
        enroll(
            Email: String!,
            Password: String!
        ): String
        createDocument(
           doc: InputDocument!
        ): [String!]
        deleteDocument(
           Name: String!,
           ThingID: String!
        ): [String!]
        createMedia(
           media: InputMediaType!
        ): [String!]
        deleteMedia(
           Name: String!,
           ThingID: String!
        ): [String!]
      }

      type Query {
        authenticate(
            Email: String!,
            Password: String!
        ): UserAuthData
        getUserinfo(
            UserID: String!,
            Email: String!
        ): UserInfo
        getDocumentAndRelated(
           Topic: String!,
           Title: String!): Document
        getDocumentByTopics(topics: [String!]): [DocumentsByTopic!]
        hello: String
        nativeHello: String
      }

    "
    ))

(def root
  (clj->js
   {:enroll (fn [data]
              (native/enroll data))
    :createDocument (fn [data]
                      (let [doc (aget data "doc")]
                        (native/createDocument doc)))
    :deleteDocument (fn [data]
                      (native/deleteDocument data))
    :createMedia (fn [data]
                   (let [media (aget data "media")]
                     (native/createMedia media)))
    :deleteMedia (fn [data]
                   (native/deleteMedia data))
    :authenticate (fn [data]
                    (native/authenticate data))
    :getUserinfo (fn [data]
                   (native/getUserinfo data))
    :getDocumentAndRelated (fn [data]
                             (native/getDocumentAndRelated data))
    :getDocumentByTopics (fn [data]
                           (let [ctx (js->clj data :keywordize-keys true)
                                 {:keys [topics]} ctx]
                             (native/getDocumentByTopics (clj->js topics))))
    :hello (fn [] "Hello World! :-)")
    :nativeHello (fn [] (native/hello))}))

(defn gql-exec
  [query callback]
  (-> (graphql schema query root)
      (.then #(callback
               (.stringify js/JSON %)))))

