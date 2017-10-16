(ns slapman.runner
    (:require [doo.runner :refer-macros [doo-tests]]
              [slapman.core-test]))

(doo-tests 'slapman.core-test)
