(ns datakam.routing)

(def routing-data
  ["/"
   ;; public
   {"" {:get
        {"" :home}}
    ;; protected
    "gql" {:post
                {"" :gql}}}
    ])
