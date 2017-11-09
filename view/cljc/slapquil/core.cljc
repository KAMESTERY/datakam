(ns slapquil.core
  #?(:clj
     (:require [quil.core :include-macros true]
               [quil.middleware]))
  #?(:cljs
     (:require [quil.core :include-macros true]
               [quil.middleware]
               [reagent.core]))
  )

#?(:clj
   (defn define-react-quil-component [name id width height sketch]
     ...))

#?(:cljs
   (defn define-react-quil-component [name id width height sketch]
     (reagent.core/create-class
       {:component-did-mount sketch

        :display-name name

        :reagent-render
        (fn [name id width height sketch]
          [:canvas {:id sketch-id
                    :width width
                    :height height}])})))

(defmacro defquil [name & {:keys [width height setup draw update params]
                           :or {width 100 height 100 params {}}}]
  `(let [sketch-id (gensym ~name)
       config (merge
                {:setup setup
                 :draw draw
                 :update update
                 :host sketch-id
                 :middleware [quil.middleware/fun-mode]}
                ~params)
       sketch (quil.core/defsketch ~name config)]
     (slapquil.core/define-react-quil-component
       ~name sketch-id width height sketch)))

(comment
  ;; https://github.com/skrat/quil-reagent-test/blob/master/src/kil/core.cljs
  ;; https://github.com/ptaoussanis/tengen/blob/master/src/taoensso/tengen/reagent.cljc
  (require '[clojure.walk])
  (require '[quil.core :as q :include-macros true])
  (require '[quil.middleware :as m])
  (require '[slapquil.core :as sq])
  (def w 400)
  (def h 400)
  (defn setup []
    {:t 1})
  (defn update [state]
    (update-in state [:t] inc))
  (defn draw [state]
    (q/background 255)
    (q/fill 0)
    (q/ellipse (rem (:t state) w) 46 55 55))
  (sq/defquil lb
           :setup setup
           :draw draw
           :update update
           :width w
           :height h)
  (macroexpand                  '(sq/defquil lb
                                          :setup setup
                                          :draw draw
                                          :update update
                                          :width w
                                          :height h))
  (macroexpand-1                 '(sq/defquil lb
                                             :setup setup
                                             :draw draw
                                             :update update
                                             :width w
                                             :height h))
  (macroexpand                  '(cmptfn :id [x] [:div x]))
  (clojure.walk/macroexpand-all '(sq/defquil lb
                                          :setup setup
                                          :draw draw
                                          :update update
                                          :width w
                                          :height h))
  (clojure.walk/macroexpand-all '(cmptfn :id [x] [:div x]))
  (clojure.walk/macroexpand-all '(def-cmptfn foo [x] [:div x]))
  )
