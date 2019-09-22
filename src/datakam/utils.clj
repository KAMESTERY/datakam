(ns datakam.utils)

(defn get-port [default]
  (try
    (or (some-> (System/getProperty "server.port") Integer/parseInt)
        default)
    (catch java.lang.NumberFormatException e
      default)))

