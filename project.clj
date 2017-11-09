(defproject slapman "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.9.0-beta2"]
                 [org.clojure/clojurescript "1.9.946"]
                 [reagent "0.8.0-alpha1"]
                 [re-frame "0.10.2"]
                 [org.clojure/core.async "0.3.443"]
                 [re-com "2.2.0-SNAPSHOT"]
                 [secretary "1.2.3"]
                 [degree9/firebase-cljs "1.3.0"] ;; Check this out: https://github.com/velveteer/crossed/blob/master/src/app/handlers.cljs
                 [quil "2.6.0"]
                 [com.taoensso/timbre "4.10.0"]]

  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-less "1.7.5"]
            [lein-ancient "0.6.12"]
            [lein-cljfmt "0.5.7"]
            [cider/cider-nrepl "0.15.1"]]

  :min-lein-version "2.5.3"

  :source-paths ["view/clj" "view/cljc"]

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
   {:dependencies [[binaryage/devtools "0.9.7"]
                   [figwheel-sidecar "0.5.15-SNAPSHOT"]
                   [com.cemerick/piggieback "0.2.2"]
                   [re-frisk "0.5.0"]]

    :plugins      [[lein-figwheel "0.5.15-SNAPSHOT"]
                   [lein-doo "0.1.8"]
                   [lein-pdo "0.1.1"]]}}

  :cljsbuild
  {:builds
   [{:id           "dev"
     :source-paths ["view/cljc" "view/cljs"]
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
     :source-paths ["view/cljc" "view/cljs"]
     :compiler     {:main            slapman.core
                    :output-to       "public/js/compiled/app.js"
                    :optimizations   :advanced
                    :closure-defines {goog.DEBUG false}
                    :pretty-print    false}}

    {:id           "test"
     :source-paths ["view/cljc" "view/cljs" "view/test/cljs"]
     :compiler     {:main          slapman.runner
                    :output-to     "public/js/compiled/test.js"
                    :output-dir    "public/js/compiled/test/out"
                    :optimizations :none}}
    ]}

  )
