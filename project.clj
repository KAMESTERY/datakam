(defproject slapman "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.908"]
                 [reagent "0.7.0"]
                 [re-frame "0.10.1"]
                 [org.clojure/core.async "0.2.391"]
                 [re-com "2.1.0"]
                 [secretary "1.2.3"]]

  :plugins [[lein-cljsbuild "1.1.5"]
            [lein-less "1.7.5"]]

  :min-lein-version "2.5.3"

  :source-paths ["view/clj"]

  :clean-targets ^{:protect false} ["public/js/compiled" "target"
                                    "test/js"]

  :figwheel {:css-dirs ["public/css"]}

  :less {:source-paths ["less"]
         :target-path  "public/css"}

  :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}

  :aliases {"dev" ["do" "clean"
                        ["pdo" ["figwheel" "dev"]
                               ["less" "auto"]]]
            "build" ["do" "clean"
                          ["cljsbuild" "once" "min"]
                          ["less" "once"]]}

  :profiles
  {:dev
   {:dependencies [[binaryage/devtools "0.9.4"]
                   [figwheel-sidecar "0.5.13"]
                   [com.cemerick/piggieback "0.2.2"]
                   [re-frisk "0.5.0"]]

    :plugins      [[lein-figwheel "0.5.13"]
                   [lein-doo "0.1.8"]
                   [lein-pdo "0.1.1"]]}}

  :cljsbuild
  {:builds
   [{:id           "dev"
     :source-paths ["view/cljs"]
     :figwheel     {:on-jsload "slapman.core/mount-root"}
     :compiler     {:main                 slapman.core
                    :output-to            "public/js/compiled/app.js"
                    :output-dir           "public/js/compiled/out"
                    :asset-path           "js/compiled/out"
                    :source-map-timestamp true
                    :preloads             [devtools.preload
                                           re-frisk.preload]
                    :external-config      {:devtools/config {:features-to-install :all}}
                    }}

    {:id           "min"
     :source-paths ["view/cljs"]
     :compiler     {:main            slapman.core
                    :output-to       "public/js/compiled/app.js"
                    :optimizations   :advanced
                    :closure-defines {goog.DEBUG false}
                    :pretty-print    false}}

    {:id           "test"
     :source-paths ["view/cljs" "view/test/cljs"]
     :compiler     {:main          slapman.runner
                    :output-to     "public/js/compiled/test.js"
                    :output-dir    "public/js/compiled/test/out"
                    :optimizations :none}}
    ]}

  )
