(ns umschreiben-clj.test-utils
  (:require [matcher-combinators.matchers :as m]
            [rewrite-clj.parser :as p]))

(defn code->node [& forms]
  (p/parse-string-all (apply str forms)))

(defn strictly [expected]
  (m/match-with [map? m/equals] expected))
