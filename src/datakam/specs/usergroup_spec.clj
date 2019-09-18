(ns datakam.specs.usergroup-spec
  (:require [clojure.set :refer [rename-keys]]
            [clojure.spec.alpha :as s]
            [datakam.specs.common-spec :as cspk]
            [com.rpl.specter :as S]))


;; UserGroups

(s/def ::UserID ::cspk/email-type)
(s/def ::GrouID ::cspk/non-null-string-type)
(s/def ::Name ::cspk/non-null-string-type)

(s/def ::usergroup-cat (s/keys :req [::GroupID]))
(s/def ::usergroup-key (s/keys :req [::GroupID ::UserID]))
(s/def ::usergroup (s/keys :req [::GroupID ::UserID ::Name]))

(s/def ::usergroup-like (s/or
                          :c ::usergroup-cat
                          :k ::usergroup-key
                          :ug ::usergroup))
(s/def ::many-usergroups-type (s/* ::usergroup-like))

;; Helpers

(defn usergroup-keys-localize [m]
  (rename-keys m {:GroupID ::GroupID :UserID ::UserID :Name ::Name}))

(defn usergroup-to-attrvals [m]
  (let [kvec (-> m keys (into []))
        resm (S/multi-transform (S/multi-path [:GroupID (S/terminal #(hash-map :S %))]
                                              [:UserID (S/terminal #(hash-map :S %))]
                                              [:Name (S/terminal #(hash-map :S %))]) m)]
    (select-keys resm kvec)))
