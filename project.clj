(defproject datakam "0.0.1-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :min-lein-version "2.0.0"
  :plugins [[lein-tools-deps "0.4.5"]
            [cider/nrepl "0.3.0"]
            [lein-cljfmt "0.6.8"]
            [lein-ancient "0.6.15"]
            [io.taylorwood/lein-native-image "0.3.1"]]
  :native-image {:name "datakam.server"                 ;; name of output image, optional
                 :opts ["--initialize-at-build-time"
                        ;; optional native image name override
                        ;;"-H:Name=server"
                        ;; pass-thru args to GraalVM native-image, optional
                        "--verbose"
                        "-H:EnableURLProtocols=http,https"
                        "--report-unsupported-elements-at-runtime" ;; ignore native-image build errors
                        "--no-server"                ;; TODO issue with subsequent builds failing on same server
                        ]}
  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :lein-tools-deps/config {:config-files [:install :user :project]
                           :resolve-aliases []}
  :profiles {:dev {:aliases {"run-dev" ["trampoline" "run" "-m" "datakam.server/run-dev"]}
                   :dependencies [[io.pedestal/pedestal.service-tools "0.5.8"]]}
             :uberjar {;;:aot [datakam.server]
                       :aot :all
                       :native-image {:jvm-opts ["-server"
                                                 "-Dclojure.compiler.direct-linking=true"]}}}
  :main datakam.server)
