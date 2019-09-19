(ns datakam.specs.macros)

(defmacro okspk? [spk data]
  `(or (clojure.spec.alpha/valid? ~spk ~data)
       (clojure.spec.alpha/explain-data ~spk ~data)))

