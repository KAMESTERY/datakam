(defproject slapman "0.0.1-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 ;; [org.clojure/clojure "1.9.0-alpha17"]
                 [org.clojure/core.async "0.3.443"]
                 [org.clojure/data.json "0.2.6"]

                 [io.pedestal/pedestal.service "0.5.2"]

                 ;; Remove this line and uncomment one of the next lines to
                 ;; use Immutant or Tomcat instead of Jetty:
                 [io.pedestal/pedestal.jetty "0.5.2"]
                 ;; [io.pedestal/pedestal.immutant "0.5.2"]
                 ;; [io.pedestal/pedestal.tomcat "0.5.2"]

                 [ch.qos.logback/logback-classic "1.2.3" :exclusions [org.slf4j/slf4j-api]]
                 [org.slf4j/jul-to-slf4j "1.8.0-alpha2"]
                 [org.slf4j/jcl-over-slf4j "1.8.0-alpha2"]
                 [org.slf4j/log4j-over-slf4j "1.8.0-alpha2"]
                 ;; [com.walmartlabs/lacinia "0.18.0" :exclusions [clojure-future-spec]]
                 [com.walmartlabs/lacinia "0.18.0"]
                 [com.walmartlabs/lacinia-pedestal "0.2.0"]
                 [clj-http "3.6.1"]
                 [com.amazonaws/aws-lambda-java-core "1.1.0"]]
  :plugins [[lein-ancient "0.6.10"]
            [lein-cljfmt "0.5.6"]]
  :min-lein-version "2.0.0"
  :resource-paths ["config", "resources"]
  ;; If you use HTTP/2 or ALPN, use the java-agent to pull in the correct alpn-boot dependency
  ;:java-agents [[org.mortbay.jetty.alpn/jetty-alpn-agent "2.0.5"]]
  :profiles {:dev {:aliases {"run-dev" ["trampoline" "run" "-m" "slapman.server/run-dev"]}
                   :dependencies [[io.pedestal/pedestal.service-tools "0.5.2"]]}
             :lambda {:main slapman.lambda
                      :uberjar-name "slapalicious.jar"
                      :aot [slapman.lambda]}
             :uberjar {:aot [slapman.server]}}
  :main ^{:skip-aot true} slapman.server)

