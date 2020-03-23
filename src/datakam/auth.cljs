(ns datakam.auth
  (:require [tick.alpha.api :as time]
            [taoensso.timbre :as log]
            [datakam.dal :as dal]
            ["dknative" :as native]))

(defn- hash-password [password]
  (hashers/encrypt password {:alg :pbkdf2+sha512 :salt (nonce/random-bytes 64)}))

(defn- check-password [hashed-password password]
  (hashers/check password hashed-password))

(defn enroll! [user]
  (let [passwd (:Password user)
        user-no-pass (dissoc user :Password)]
    (if passwd
      (try
        (when-not (empty? (dal/get-user user-no-pass))
          {:errmsg "User Already Exists"})
        (catch AssertionError e
          (-> user-no-pass
              (assoc :PasswordHash (hash-password passwd))
              dal/put-user)
          {:msg "Success"}))
      {:errmsg "Password has no Value"})))

(def token-ttl (time/new-duration 4 :days))
;;(def token-ttl (time/new-duration 4 :seconds))

(defn jwt-encode [claims]
  (-> claims
      (assoc :exp (time/+ (time/now) token-ttl))
      (jwt/sign ec-privkey {:alg :es512})))

(defn jwt-decode [token]
  (-> token
      (jwt/unsign ec-pubkey {:alg :es512
                             :now (time/minus (time/now) token-ttl)})))

(defn jwt-refresh [token]
  (-> token
      jwt-decode
      (assoc :exp (time/plus (time/now) token-ttl))
      jwt-encode))

(defn authenticate [creds]
  (let [passwd (:Password creds)
        creds-no-pass (dissoc creds :Password)]
    (if passwd
      (try
        (let [existing-user (dal/get-user creds-no-pass)]
          (when (and
                 (-> existing-user empty? not)
                 (check-password (:PasswordHash existing-user) passwd))
            (let [token (-> existing-user
                            (select-keys [:UserID :Email
                                          :Username :Role
                                          :Confirmed])
                            jwt-encode)]
              {:token token})))
        (catch AssertionError e
          {:errmsg "Could not Authenticate User"}))
      {:errmsg "Password has no Value"})))

(comment
  ;;;; AUTH
  (def userid "some-userid")
  (def passwd "some-password")
  (def hpasswd (hash-password passwd))
  (log/debug hpasswd)
  (check-password hpasswd passwd)
  (check-password hpasswd "some-other-password")
  ;;;; JWT
  (def claims {:UserID "sf@sdgf.fg" :Role 9874})
  (def token (jwt-encode claims))
  (log/debug token)
  (jwt-decode token))

