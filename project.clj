(defproject datakam "0.0.1-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :min-lein-version "2.0.0"
  :plugins [[lein-tools-deps "0.4.5"]
            [cider/nrepl "0.3.0"]
            [lein-cljfmt "0.6.4"]
            [lein-ancient "0.6.15"]
            [io.taylorwood/lein-native-image "0.3.1"]]
  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :lein-tools-deps/config {:config-files [:install :user :project]
                           :resolve-aliases []}
  :profiles {:dev {:aliases {"run-dev" ["trampoline" "run" "-m" "datakam.server/run-dev"]}
                   :dependencies [[io.pedestal/pedestal.service-tools "0.5.7"]]}
             :uberjar {;;:aot [datakam.server]
                       :aot :all}}
  :main datakam.server)
