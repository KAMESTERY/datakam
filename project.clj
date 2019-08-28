(defproject datakam "0.0.1-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [io.pedestal/pedestal.service "0.5.7"]

                 ;; Remove this line and uncomment one of the next lines to
                 ;; use Immutant or Tomcat instead of Jetty:
                 ;;[io.pedestal/pedestal.jetty "0.5.7"]
                 [io.pedestal/pedestal.immutant "0.5.7"]
                 ;; [io.pedestal/pedestal.tomcat "0.5.7"]
                 [io.pedestal/pedestal.aws "0.5.7"]
                 [com.cognitect.aws/api "0.8.352"]
                 [com.cognitect.aws/endpoints "1.1.11.607"]
                 [com.cognitect.aws/dynamodb "726.2.484.0"] ;; https://mvnrepository.com/artifact/com.cognitect.aws/dynamodb
                 [ch.qos.logback/logback-classic "1.3.0-alpha4" :exclusions [org.slf4j/slf4j-api]]
                 [org.slf4j/jul-to-slf4j "2.0.0-alpha0"]
                 [org.slf4j/jcl-over-slf4j "2.0.0-alpha0"]
                 [org.slf4j/log4j-over-slf4j "2.0.0-alpha0"]
                 [environ "1.1.0"]
                 [com.walmartlabs/lacinia "0.34.0"]
                 [selmer "1.12.14"]
                 [com.rpl/specter "1.1.2"]
                 [org.clojure/test.check "0.10.0" :scope "test"]]
  :min-lein-version "2.0.0"
  ;;:jvm-opts ["-server"
  ;;           "-Dclojure.compiler.elide-meta=\"[:doc :file :line :added]\""
  ;;           "-Dclojure.compiler.direct-linking=true"]
  :resource-paths ["config", "resources"]
  ;;:jvm-opts ["-server" "-Xmx12G" "-Xmx8g" "-XX:+UseG1GC"]
  :plugins [[cider/nrepl "0.3.0"]
            [lein-cljfmt "0.6.4"]
            [lein-ancient "0.6.15"]
            [io.taylorwood/lein-native-image "0.3.1"]]
  :native-image {;;:name "my-app"                 ;; name of output image, optional
                 ;;:graal-bin "/path/to/graalvm/" ;; path to GraalVM home, optional
                 :opts ["--verbose"
                        "-J-server" "-J-Xmx12G" "-J-Xmx8g" "-J-XX:+UseG1GC"
                        "-H:EnableURLProtocols=http,https"
                        "--report-unsupported-elements-at-runtime" ;; ignore native-image build errors
                        "--initialize-at-build-time"
                        "--no-server" ;; TODO issue with subsequent builds failing on same server
                        "--verbose"]
                 :name "server"}           ;; pass-thru args to GraalVM native-image, optional
  ;; If you use HTTP/2 or ALPN, use the java-agent to pull in the correct alpn-boot dependency
                                        ;:java-agents [[org.mortbay.jetty.alpn/jetty-alpn-agent "2.0.5"]]
  :profiles {:dev {:aliases {"run-dev" ["trampoline" "run" "-m" "datakam.server/run-dev"]}
                   :dependencies [[io.pedestal/pedestal.service-tools "0.5.7"]]}
             :uberjar {:aot [datakam.server]}}
  :main ^{:skip-aot true} datakam.server)
