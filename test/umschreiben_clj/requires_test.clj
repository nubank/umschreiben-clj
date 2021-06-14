(ns umschreiben-clj.requires-test
  (:refer-clojure :exclude [replace])
  (:require [clojure.test :refer [are deftest is testing]]
            [matcher-combinators.matchers :as m]
            [matcher-combinators.test :refer [match?]]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]
            [umschreiben-clj.requires :as requires]))

(defn code->node [& forms]
  (p/parse-string-all (apply str forms)))

(defn- strictly [expected]
  (m/match-with [map? m/equals] expected))

(deftest use-require-test
  (are [require symbol result]
       (match? (strictly result) (requires/use-require (#'requires/require-node->require-map (code->node require)) symbol))

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

(deftest replace-using-require-test
  (are [code symbol result]
       (match? (strictly result)
               (n/sexpr
                (requires/replace-using-require
                 identity
                 (code->node code)
                 (#'requires/require-node->require-map (code->node '[a.b.c :as c]))
                 symbol)))

    '(myfn 1 2)          #{'myfn}  '(c/myfn 1 2)
    '(apply myfn [1 2])  #{'myfn}  '(apply c/myfn [1 2])

    '(ns/myfn 1 2)          #{'ns/myfn}  '(c/myfn 1 2)
    '(apply ns/myfn [1 2])  #{'ns/myfn}  '(apply c/myfn [1 2])

    '(apply ns/myfn [ns/value 2])  #{'ns/myfn}  '(apply c/myfn [ns/value 2])))

(deftest replace-test
  (testing "effect on 'ns' code"
    (are [code from new result]
         (match? (strictly result)
                 (some-> (requires/replace (code->node code) from new) n/sexpr))

      '(ns aa (:require [x.y.z]))  'x.y.z  'x.y.z  '(ns aa (:require x.y.z))
      '(ns aa (:require x.y.z))    'x.y.z  'x.y.z  '(ns aa (:require x.y.z))

      '(ns aa (:require a.b.c))            'a.b.c  'x.y.z  '(ns aa (:require x.y.z))
      '(ns aa (:require [a.b.c]))          'a.b.c  'x.y.z  '(ns aa (:require x.y.z))
      '(ns aa (:require [a.b.c :as c]))    'a.b.c  'x.y.z  '(ns aa (:require x.y.z))
      '(ns aa (:require [a.b.c] [e.f.g]))  'a.b.c  'x.y.z  '(ns aa (:require x.y.z [e.f.g]))

      '(ns aa)                 'a.b.c  'x.y.z  nil?
      '(ns aa (:using e.f.g))  'a.b.c  'x.y.z  nil?
      '(ns aa (:using a.b.c))  'a.b.c  'x.y.z  nil?))

  (testing "replaces when the symbol is on :refer list"
    (is (match? '(do (ns aa (:require [x.y.z :as z])) (def bb (z/myfn 1)))
                (n/sexpr (requires/replace
                          (code->node '(ns aa (:require [a.b.c :as c :refer [myfn]]))
                                      '(def bb (myfn 1)))
                          'a.b.c
                          '[x.y.z :as z])))))

  (testing "replaces and refer a symbol"
    (is (match? '(do (ns aa (:require [x.y.z :refer [myfn]])) (myfn 1))
                (n/sexpr (requires/replace
                          (code->node '(ns aa (:require [a.b.c :as a]))
                                      '(a/myfn 1))
                          'a.b.c
                          '[x.y.z :refer [myfn]])))))

  ; breaking
  #_(testing "replaces a symbol referred through :all"
      (is (match? '(do (ns aa (:require [x.y.z :as x])) (x/myfn 1))
                  (n/sexpr (requires/replace
                            (code->node '(ns aa (:require [a.b.c :refer :all]))
                                        '(myfn 1))
                            'a.b.c
                            '[x.y.z :as x])))))

  (testing "replaces symbols using default replace-fn"
    (is (match? (strictly '(do (ns aa (:require [x.y.z :as z])) (def bb (z/myfn 1))))
                (n/sexpr (requires/replace
                          (code->node '(ns aa (:require [a.b.c :as c])) '(def bb (c/myfn 1)))
                          'a.b.c
                          '[x.y.z :as z])))))

  (testing "replaces multiple symbols using default replace-fn"
    (is (match? (strictly '(do (ns aa (:require [x.y.z :as z])) (def bb (z/myfn 1) (z/myfn 2))))
                (n/sexpr (requires/replace
                          (code->node '(ns aa (:require [a.b.c :as c])) '(def bb (c/myfn 1) (c/myfn 2)))
                          'a.b.c
                          '[x.y.z :as z])))))

  (testing "replaces multiple symbols using custom replace-fn"
    (is (match? (strictly '(do (ns aa (:require [x.y.z :as z])) (def bb (replaced true))))
                (n/sexpr (requires/replace
                          (code->node '(ns aa (:require [a.b.c :as c])) '(def bb (c/myfn 2)))
                          'a.b.c
                          '[x.y.z :as z]
                          (constantly '(replaced true)))))))

  (testing "replaces multiple symbols in the nested list (function call) "
    (is (match? '(do (ns aa (:require [x.y.z :as z])) (def bb (z/myfn (z/another-fn 1))))
                (n/sexpr (requires/replace
                          (code->node '(ns aa (:require a.b.c))
                                      '(def bb (a.b.c/myfn (a.b.c/another-fn 1))))
                          'a.b.c
                          '[x.y.z :as z])))))

  (testing "replaces multiple symbols in the same list (function call) "
    (is (match? '(do (ns aa (:require [x.y.z :as z])) (def bb (z/myfn z/another-fn)))
                (n/sexpr (requires/replace
                          (code->node '(ns aa (:require a.b.c))
                                      '(def bb (a.b.c/myfn a.b.c/another-fn)))
                          'a.b.c
                          '[x.y.z :as z])))))

  (testing "replaces multiple symbols with nested function calls"
    (is (match? '(do (ns aa (:require [x.y.z :as z])) (def bb (z/myfn z/some-value (z/another-fn z/some-value))))
                (n/sexpr (requires/replace
                          (code->node '(ns aa (:require a.b.c))
                                      '(def bb (a.b.c/myfn a.b.c/some-value (a.b.c/another-fn a.b.c/some-value))))
                          'a.b.c
                          '[x.y.z :as z]))))))

(deftest replacing-in-anonymous-functions
  (testing "replace works fine when symbol is inside a map"
    (is (match?
         "(ns aa (:require [x.y.z :as z])) (def something '#(z/myfn 1))"
         (n/string
          (requires/replace
           (p/parse-string-all "(ns aa (:require a.b.c)) (def something '#(a.b.c/myfn 1))")
           'a.b.c
           '[x.y.z :as z]))))))

(deftest replacing-in-collections
  (testing "replace works fine when symbol is inside a vector"
    (is (match?
         '(do (ns aa (:require [x.y.z :as z]))
              (def something [z/element :element]))
         (n/sexpr (requires/replace
                   (code->node '(ns aa (:require a.b.c))
                               '(def something [z/element :element]))
                   'a.b.c
                   '[x.y.z :as z])))))

  (testing "replace works fine when symbol is inside a set"
    (is (match?
         '(do (ns aa (:require [x.y.z :as z]))
              (def something #{z/element :element}))
         (n/sexpr (requires/replace
                   (code->node '(ns aa (:require a.b.c))
                               '(def something #{a.b.c/element :element}))
                   'a.b.c
                   '[x.y.z :as z])))))

  (testing "replace works fine when symbol is inside a map"
    (is (match?
         '(do (ns aa (:require [x.y.z :as z]))
              (def something {z/key :value
                              :key  z/value}))
         (n/sexpr (requires/replace
                   (code->node '(ns aa (:require a.b.c))
                               '(def something {a.b.c/key :value
                                                :key      a.b.c/value}))
                   'a.b.c
                   '[x.y.z :as z]))))))

(deftest replacing-in-binding-forms
  (testing "replace works fine in right-hand side of binding form"
    (let [input    "(ns aa (:require a.b.c)) (let [x a.b.c/myfn] (x 1))"
          expected "(ns aa (:require [x.y.z :as z])) (let [x z/myfn] (x 1))"]
      (is (= expected
             (n/string (requires/replace
                        (p/parse-string-all input)
                        'a.b.c
                        '[x.y.z :as z])))))))

(deftest replace-refer-by-refer
  (testing "replace works if we are replacing a symbol that is referred"
    (is (match? '(do (ns aa (:require [x.y.z :refer [myfn another-fn]])) (myfn 1))
                (n/sexpr (requires/replace
                          (code->node '(ns aa (:require [a.b.c :refer [another-fn myfn]]))
                                      '(myfn 1))
                          'a.b.c
                          '[x.y.z :refer [myfn another-fn]]))))))

(comment
  ; We are not going to support this use case for now.
  (deftest replacing-refer-with-refer
    (testing "able to change the base namespace of a :refer"
      (is (match? '(do (ns aa (:require [x.y.z :refer [myfn]])) (myfn 1))
                  (n/sexpr (requires/replace
                            (code->node '(ns aa (:require [a.b.c :refer [myfn]]))
                                        '(myfn 1))
                            'a.b.c
                            'x.y.z))))))

  (deftest known-issues
    ; This can't happen actually, it could if we support replacing refers, for that we should provide an extra bit of information
    ; like this {'myfn 'myfn-shadow}.
    (testing "warning: it is possible to introduce shadowing"
      ;; to fix this we would have to collect scope as we traverse
      (is (match? '(do (ns aa (:require [x.y.z :refer [myfn-shadow]])) (let [myfn-shadow 1] (myfn 1)))
                  (n/sexpr (requires/replace
                            (code->node '(ns aa (:require [a.b.c :refer [myfn]]))
                                        '(let [myfn-shadow inc] (myfn 1)))
                            'a.b.c
                            '[x.y.z :refer [myfn-shadow]]))))))

  (testing "don't replace :refer when shadowed by (let) binding"
    (is (match? '(do (ns aa (:require [x.y.z :refer [myfn-shadow]])) (let [myfn-shadow 1] (inc myfn-shadow)))
                (n/sexpr (requires/replace
                          (code->node '(ns aa (:require [a.b.c :refer [myfn]]))
                                      '(let [myfn 1] (inc myfn)))
                          'a.b.c
                          '[x.y.z :refer [myfn-shadow]])))))

  (testing "don't replace :refer when shadowed by (fn) binding"
    (is (match? '(do (ns aa (:require [x.y.z :as z])) (fn [myfn] (inc myfn)))
                (n/sexpr (requires/replace
                          (code->node '(ns aa (:require [a.b.c :refer [myfn]]))
                                      '(fn [myfn] (inc myfn)))
                          'a.b.c
                          '[x.y.z :as z]))))))

(deftest add-test
  (testing "effect on 'ns' code"
    (are [current new result]
         (match? (strictly result)
                 (n/sexpr (requires/add (code->node current) new)))

      '(ns aa (:require [a.b.c]))  'x.y.z  '(ns aa (:require x.y.z [a.b.c]))
      '(ns aa (:require [x.y.z]))  'x.y.z  '(ns aa (:require x.y.z))
      '(ns aa (:using a.b.c))      'x.y.z  '(ns aa (:using a.b.c) (:require x.y.z))
      '(ns aa)                     'x.y.z  '(ns aa (:require x.y.z))
      '(ns aa (:using e.f.g))      'x.y.z  '(ns aa (:using e.f.g) (:require x.y.z))
      '(ns aa (:using a.b.c))      'x.y.z  '(ns aa (:using a.b.c) (:require x.y.z))))

  (testing "if the namespace is already required behaves like `replace`"
    (let [namespace (code->node '(ns aa (:require [a.b.c :as c])) '(def bb (c/myfn 2)))]
      (is (match? '(do (ns aa (:require [a.b.c :refer [myfn]])) (def bb (myfn 2)))
                  (n/sexpr (requires/add namespace '[a.b.c :refer [myfn]]))))
      (is (= (n/sexpr (requires/replace namespace 'a.b.c '[a.b.c :refer [myfn]]))
             (n/sexpr (requires/add namespace '[a.b.c :refer [myfn]]))))))

  (testing "add a new require"
    (is (match? (strictly '(do (ns aa (:require [x.y.z :as z] [a.b.c :as c])) (def bb (c/myfn 1))))
                (n/sexpr (requires/add
                          (code->node '(ns aa (:require [a.b.c :as c])) '(def bb (c/myfn 1)))
                          '[x.y.z :as z]))))))
