(ns umschreiben-clj.internals.requires-test
  (:require [clojure.test :refer [are deftest is testing]]
            [matcher-combinators.test :refer [match?]]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]
            [umschreiben-clj.internals.requires :as internals.requires]
            [umschreiben-clj.test-utils :refer [code->node strictly]]))

(deftest use-require-test
  (are [require symbol result]
       (match? (strictly result) (internals.requires/use-require (internals.requires/require-node->require-map (code->node require)) symbol))

    '[a.b.c]                         'myfn  'a.b.c/myfn
    'a.b.c                           'myfn  'a.b.c/myfn
    '[a.b.c :as c]                   'myfn  'c/myfn
    '[a.b.c :refer [myfn]]           'myfn  'myfn
    '[a.b.c :as c :refer [myfn]]     'myfn  'myfn
    '[a.b.c :as c :refer [otherfn]]  'myfn  'c/myfn

    '[a.b.c]                         'ns/myfn  'a.b.c/myfn
    'a.b.c                           'ns/myfn  'a.b.c/myfn
    '[a.b.c :as c]                   'ns/myfn  'c/myfn
    '[a.b.c :refer [myfn]]           'ns/myfn  'myfn
    '[a.b.c :as c :refer [myfn]]     'ns/myfn  'myfn
    '[a.b.c :as c :refer [otherfn]]  'ns/myfn  'c/myfn

    '[a.b.c :as c]  'a.b.c/myfn  'c/myfn))

