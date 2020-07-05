(ns datakam.service
  (:require [clojure.pprint :refer [pprint]]
            [io.pedestal.http :as http]
            [io.pedestal.http.route :as route]
            [io.pedestal.http.body-params :as body-params]
            [io.pedestal.interceptor.error :as error-int]
            [ring.util.response :as ring-resp]
            [selmer.parser :as selmer]
            [com.walmartlabs.lacinia :refer [execute]]
            [com.walmartlabs.lacinia.util :as lutil]
            [taoensso.timbre :as log]
            [datakam.gql.schema :refer [main-schema]]
            [datakam.dal :as dal]
            [datakam.domain :as dmn]
            [datakam.utils :as utils]))

(defn about-page
  [request]
  (ring-resp/response (format "Clojure %s - served from %s"
                              (clojure-version)
                              (route/url-for ::about-page))))

(defn home-page
  [request]
  (ring-resp/response "Hello World!"))

(defn list-tables
  [request]
  (ring-resp/response (dal/list-tables)))

(defn get-topic
  [request]
  (let [namespace (-> request :path-params :namespace)
        topic (-> request :path-params :topic)]
    (ring-resp/response (dmn/query-document {:Name (str namespace ":##:" topic)}))))

(defn get-title
  [request]
  (let [namespace (-> request :path-params :namespace)
        topic (-> request :path-params :topic)
        title (-> request :path-params :title)]
    (ring-resp/response (dmn/get-document
                         {:Topic (str namespace ":##:" topic)
                          :DocumentID (str namespace ":##:" topic ":##:" title)}))))

(defn graphiql
  [request]
  (ring-resp/response
   (selmer/render-file "public/graphiql.html"
                       {:url (route/url-for ::gql)})))

(defn gql
  [request]
  (log/debug "GQL Request:" (-> request :json-params :query))
  (let [query (-> request :json-params :query)
        result (try (execute main-schema query nil nil)
                    ;(catch java.lang.AssertionError e
                    ;  (log/debug "Catching Exception")
                    ;  (pprint e)
                    ;  (-> e Throwable->map :via first))
                    ;(catch java.lang.Exception e
                    ;  (log/debug "Catching Exception")
                    ;  (pprint e)
                    ;  (-> e Throwable->map :via first))
                    (catch java.lang.AssertionError e
                      {:status  400
                       :headers {}
                       :body    (lutil/as-error-map e)})
                    (catch java.lang.Exception e
                      {:status  500
                       :headers {}
                       :body    (lutil/as-error-map e)}))]
    ;(pprint result)
    (log/debug "GQL Response:" result)
    (ring-resp/response result)))

;(defn gql
;  [request]
;  (log/debug "GQL Request:" (-> request :json-params :query))
;  (let [query (-> request :json-params :query)
;        result (execute main-schema query nil nil)]
;    (log/debug "GQL Response:" result)
;    (ring-resp/response result)))

;; Defines "/" and "/about" routes with their associated :get handlers.
;; The interceptors defined after the verb map (e.g., {:get home-page}
;; apply to / and its children (/about).
(def common-interceptors [(body-params/body-params) http/html-body])

(def json-interceptors [(body-params/body-params) http/json-body])

;(def service-error-handler
;  (error-int/error-dispatch [ctx ex]
;
;                            [{:exception-type java.lang.AssertionError}]
;                            (assoc ctx :response {:status 400
;                                                  :headers {}
;                                                  :body (lutil/as-error-map ex)})
;
;                            [{:exception-type java.lang.Exception}]
;                            (assoc ctx :response {:status 500
;                                                  :headers {}
;                                                  :body (lutil/as-error-map ex)})
;
;                            :else
;                            (assoc ctx :io.pedestal.interceptor.chain/error ex)))

;; Tabular routes
(def routes #{["/" :get (conj common-interceptors `home-page)]
              ["/about" :get (conj common-interceptors `about-page)]
              ["/list-tables" :get (conj json-interceptors `list-tables)]
              ["/topic/:namespace/:topic" :get (conj json-interceptors `get-topic)]
              ["/title/:namespace/:topic/:title" :get (conj json-interceptors `get-title)]
              ["/gql" :post (conj json-interceptors `gql)]
              ;["/gql" :post (conj json-interceptors service-error-handler `gql)]
              ["/graphiql" :get (conj common-interceptors `graphiql)]})

;; Map-based routes
                                        ;(def routes `{"/" {:interceptors [(body-params/body-params) http/html-body]
                                        ;                   :get home-page
                                        ;                   "/about" {:get about-page}}})

;; Terse/Vector-based routes
                                        ;(def routes
                                        ;  `[[["/" {:get home-page}
                                        ;      ^:interceptors [(body-params/body-params) http/html-body]
                                        ;      ["/about" {:get about-page}]]]])

;; Consumed by datakam.server/create-server
;; See http/default-interceptors for additional options you can configure
(def service {:env :prod
              ;; You can bring your own non-default interceptors. Make
              ;; sure you include routing and set it up right for
              ;; dev-mode. If you do, many other keys for configuring
              ;; default interceptors will be ignored.
              ;; ::http/interceptors []
              ::http/routes routes

              ;; Uncomment next line to enable CORS support, add
              ;; string(s) specifying scheme, host and port for
              ;; allowed source(s):
              ;;
              ;; "http://localhost:8080"
              ;;
              ;;::http/allowed-origins ["scheme://host:port"]

              ;; Tune the Secure Headers
              ;; and specifically the Content Security Policy appropriate to your service/application
              ;; For more information, see: https://content-security-policy.com/
              ;;   See also: https://github.com/pedestal/pedestal/issues/499
              ;;::http/secure-headers {:content-security-policy-settings {:object-src "'none'"
              ;;                                                          :script-src "'unsafe-inline' 'unsafe-eval' 'strict-dynamic' https: http:"
              ;;                                                          :frame-ancestors "'none'"}}

              ;; Root for resource interceptor that is available by default.
              ::http/resource-path "/public"

              ;; Either :jetty, :immutant or :tomcat (see comments in project.clj)
              ;;  This can also be your own chain provider/server-fn -- http://pedestal.io/reference/architecture-overview#_chain_provider
              ::http/type :jetty
              ;;::http/type :immutant
              ;;::http/host "localhost"
              ::http/host "0.0.0.0"
              ::http/port (utils/get-port 8080)
              ;; Options to pass to the container (Jetty)
              ::http/container-options {:h2c? true
                                        :h2? false
                                        ;:keystore "test/hp/keystore.jks"
                                        ;:key-password "password"
                                        ;:ssl-port 8443
                                        :ssl? false
                                        ;; Alternatively, You can specify you're own Jetty HTTPConfiguration
                                        ;; via the `:io.pedestal.http.jetty/http-configuration` container option.
                                        ;:io.pedestal.http.jetty/http-configuration (org.eclipse.jetty.server.HttpConfiguration.)
                                        }})

