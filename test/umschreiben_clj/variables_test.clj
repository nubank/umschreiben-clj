(ns umschreiben-clj.variables-test
  (:require [clojure.test :refer [are deftest is testing]]
            [matcher-combinators.test :refer [match? thrown-match?]]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]
            [rewrite-clj.zip :as z]
            [umschreiben-clj.test-utils :refer [code->node]]
            [umschreiben-clj.variables :as variables]))

(defn- build-code [requires body]
  [`(~'ns hello.world (:require ~@requires))
   `(def ~'ignore whatever/replace)
   `(def ~'o-brave ~body)
   `(def ~'ignore-again whatever/replace)])

(defn build-node [requires body]
  (code->node (build-code requires body)))

(deftest basic
  (testing "rename to other name but same namespace"
    (is (match? (build-code ['clojure.string] 'clojure.string/upper-case)
                (n/sexpr (variables/rename (build-node ['clojure.string]
                                                       'clojure.string/lower-case)
                                           'clojure.string/lower-case
                                           'upper-case
                                           'clojure.string)))))

  (testing "rename to other name and other namespace"
    (is (match? (build-code ['[clojure.string :as str] 'other.namespace]
                            '(other.namespace/upper-case (str/upper-case "prueba")))
                (n/sexpr (variables/rename (build-node ['[clojure.string :as str]]
                                                       '(str/lower-case (str/upper-case "prueba")))
                                           'clojure.string/lower-case
                                           'upper-case
                                           'other.namespace)))))

  (testing "rename to other name and other namespace with aliasing"
    (is (match? (build-code ['[clojure.string :as str]
                             '[other.namespace :as other]]
                            '(other/upper-case (str/upper-case "prueba")))
                (n/sexpr (variables/rename (build-node ['[clojure.string :as str]]
                                                       '(str/lower-case (str/upper-case "prueba")))
                                           'clojure.string/lower-case
                                           'upper-case
                                           '[other.namespace :as other])))))

  (testing "rename to same namespace retaining previous alias"
    (is (match? (build-code ['[clojure.string :as str]]
                            '(str/upper-case (str/upper-case "prueba")))
                (n/sexpr (variables/rename (build-node ['[clojure.string :as str]]
                                                       '(str/lower-case (str/upper-case "prueba")))
                                           'clojure.string/lower-case
                                           'upper-case
                                           '[clojure.string :as ignored-str-alias])))))

  (testing "adding an `:as` leaves `:refer` as is"
    (is (match? (build-code ['[clojure.string :as str]
                             '[other.namespace :as new-other :refer [something]]]
                            '(new-other/upper-case (str/upper-case "prueba")))
                (n/sexpr (variables/rename (build-node ['[clojure.string :as str]
                                                        '[other.namespace :refer [something]]]
                                                       '(str/lower-case (str/upper-case "prueba")))
                                           'clojure.string/lower-case
                                           'upper-case
                                           '[other.namespace :as new-other])))))

  (testing "adding an `:as` when the body is unchanged shouldn't remove existing `:refer`"
    (is (match? (build-code ['[clojure.string :as str]
                             '[other.namespace :as new-other :refer [lower-case]]]
                            '(lower-case (str/upper-case "prueba")))
                (n/sexpr (variables/rename (build-node ['[clojure.string :as str]
                                                        '[other.namespace :as new-other :refer [lower-case]]]
                                                       '(lower-case (str/upper-case "prueba")))
                                           'clojure.string/whatever
                                           'whatever
                                           '[other.namespace :as new-other])))))

  (testing "refer takes precedence over alias"
    (is (match? (build-code ['[clojure.string :as str]
                             '[other.namespace :as new-other :refer [upper-case]]]
                            '(upper-case (str/upper-case "prueba")))
                (n/sexpr (variables/rename (build-node ['[clojure.string :as str]
                                                        '[other.namespace :refer [upper-case]]]
                                                       '(str/lower-case (str/upper-case "prueba")))
                                           'clojure.string/lower-case
                                           'upper-case
                                           '[other.namespace :as new-other])))))

  (testing "rename to other name in other namespace that is already in :require. In this case `:alias-ns` is ignored(?)"
    (is (match? (build-code ['[clojure.string :as str]
                             '[other.namespace :as other]]
                            '(other/upper-case (str/upper-case "prueba")))
                (n/sexpr (variables/rename (build-node ['[clojure.string :as str]
                                                        '[other.namespace :as other]]
                                                       '(str/lower-case (str/upper-case "prueba")))
                                           'clojure.string/lower-case
                                           'upper-case
                                           '[other.namespace :as 'new-other])))))

  (testing "rename to other name using :refer"
    (is (match? (build-code ['[clojure.string :as str :refer [upper-case]]]
                            '(upper-case lower-case))
                (n/sexpr (variables/rename (build-node ['[clojure.string :as str]]
                                                       '(str/lower-case lower-case))
                                           'clojure.string/lower-case
                                           'upper-case
                                           '[clojure.string :refer [upper-case]]))))) ;; `:refer` to do a refer?

  (testing "rename to other name in other namespace using :refer"
    (is (match? (build-code ['[clojure.string :as str]
                             '[other.namespace :refer [upper-case]]]
                            '(upper-case lower-case))
                (n/sexpr (variables/rename (build-node ['[clojure.string :as str]]
                                                       '(str/lower-case lower-case))
                                           'clojure.string/lower-case
                                           'upper-case
                                           '[other.namespace :refer [upper-case]])))))

  ; breaking
  #_(testing "use post-fn function when present"
      (is (match? (build-code ['[a.b.c :as a]]
                              '(a/some-fn))
                  (n/sexpr (variables/rename (build-node ['[a.b.c :as a]]
                                                         'a/some-value)
                                             'a.b.c/some-value
                                             'a.b.c/some-fn
                                             {:post-fn (fn [x] "TODO")})))))

  (testing "rename to other name in other namespace using :refer that is already in :require"
    (is (match? (build-code ['[clojure.string :as str]
                             '[other.namespace :as other :refer [upper-case]]]
                            '(upper-case lower-case))
                (n/sexpr (variables/rename (build-node ['[clojure.string :as str]
                                                        '[other.namespace :as other]]
                                                       '(str/lower-case lower-case))
                                           'clojure.string/lower-case
                                           'upper-case
                                           '[other.namespace :refer [upper-case]])))))

  (testing "preserves the refer :all"
    (is (match? (build-code ['[clojure.string :as str]
                             '[other.namespace :as other :refer :all]]
                            '(upper-case lower-case))
                (n/sexpr (variables/rename (build-node ['[clojure.string :as str]
                                                        '[other.namespace :refer :all]]
                                                       '(str/lower-case lower-case))
                                           'clojure.string/lower-case
                                           'upper-case
                                           '[other.namespace :as other :refer [upper-case]])))))

  (testing "replacing in presence of a `:refer :all` that isn't used"
    (is (match? (build-code ['[clojure.string :as str :refer :all]
                             '[other.namespace :as other]]
                            '(other/upper-case "asdf"))
                (n/sexpr (variables/rename (build-node ['[clojure.string :as str :refer :all]]
                                                       '(str/lower-case "asdf"))
                                           'clojure.string/lower-case
                                           'upper-case
                                           '[other.namespace :as other])))))

  (testing "removes the symbol from :refer to avoid clashes"
    (is (match? (build-code ['[clojure.string :as str]
                             '[other.namespace :refer [lower-case]]]
                            '(lower-case "prueba"))
                (n/sexpr (variables/rename (build-node ['[clojure.string :as str :refer [lower-case]]]
                                                       '(lower-case "prueba"))
                                           'clojure.string/lower-case
                                           'lower-case
                                           '[other.namespace :refer [lower-case]])))))

  (testing "removes the symbol from :refer but don't add other require if body doesn't change"
    (is (match? (build-code ['[clojure.string :as str]]
                            '(str/upper-case "prueba"))
                (n/sexpr (variables/rename (build-node ['[clojure.string :as str :refer [lower-case]]]
                                                       '(str/upper-case "prueba"))
                                           'clojure.string/lower-case
                                           'lower-case
                                           '[other.namespace :refer [lower-case]])))))

  (testing "removes the :refer to avoid clashes"
    (is (match? (build-code ['[clojure.string :as str :refer [upper-case]]
                             '[other.namespace :refer [lower-case]]]
                            '(lower-case "prueba"))
                (n/sexpr (variables/rename (build-node ['[clojure.string :as str :refer [upper-case lower-case]]]
                                                       '(lower-case "prueba"))
                                           'clojure.string/lower-case
                                           'lower-case
                                           '[other.namespace :refer [lower-case]])))))

  (testing "don't replace reader macros"
    (is (match? (build-code ['[clojure.string :as str]
                             '[other.namespace :as other]]
                            ;; read-string turns into the proper reader macro at emission time
                            '(other/uuid (read-string "#uuid \"cdb53ed3-f553-4c46-9395-349f32ad3d3f\"")))
                (n/sexpr (variables/rename (build-node ['[clojure.string :as str :refer [uuid]]]
                                                       '(uuid #uuid "cdb53ed3-f553-4c46-9395-349f32ad3d3f"))
                                           'clojure.string/uuid
                                           'uuid
                                           '[other.namespace :as other])))))

  (testing "vectors are preserved for `:refer`s"
    (let [input    "(ns aa (:require [clojure.string :as str :refer [upper-case lower-case]])) (lower-case \"prueba\")"
          expected "(ns aa (:require [clojure.string :as str :refer [upper-case]]\n [other.namespace :refer [lower-case]])) (lower-case \"prueba\")"]
      (is (= expected
             (n/string (variables/rename
                        (p/parse-string-all input)
                        'clojure.string/lower-case
                        'lower-case
                        '[other.namespace :refer [lower-case]]))))))

  (testing "do renaming inside reader comment blocks"
    (is (match?
         "(ns aa (:require a.b.c\n [other.namespace :as other])) (def something #_(other/myfn 1))"
         (n/string (variables/rename (p/parse-string-all "(ns aa (:require a.b.c)) (def something #_(a.b.c/myfn 1))")
                                     'a.b.c/myfn
                                     'myfn
                                     '[other.namespace :as other])))))

  (testing "don't add imports if body doesn't change"
    (is (match? (build-code ['[clojure.string :as str]]
                            '(str/lower-case "c"))
                (n/sexpr (variables/rename (build-node ['[clojure.string :as str]]
                                                       '(str/lower-case "c"))
                                           'clojure.string/upper-case
                                           'another-case
                                           '[other.namespace :as other])))))

  (testing "throw an exception if the symbol to replace is being referred through all"
    (is (thrown-match? clojure.lang.ExceptionInfo
                       {:referrals-to-remove ['lower-case]}
                       (n/sexpr (variables/rename (build-node ['[clojure.string :as str :refer :all]]
                                                              '(lower-case "prueba"))
                                                  'clojure.string/lower-case
                                                  'lower-case
                                                  '[other.namespace :refer [lower-case]])))))

  ; breaking
  #_(testing "throw an exception if the symbol to replace is being referred in a different require"
      (is (thrown-match? clojure.lang.ExceptionInfo
                         {}
                         (n/sexpr (variables/rename (build-node ['[clojure.string :as str]
                                                                 '[a.namespace :refer [lower-case]]]
                                                                '(str/lower-case "prueba"))
                                                    'clojure.string/lower-case
                                                    'lower-case
                                                    '[other.namespace :refer [lower-case]])))))

  ; breaking
  #_(testing "throw an exception if the symbol to replace is being referred through all in a different require"
      (is (thrown-match? clojure.lang.ExceptionInfo
                         {}
                         (n/sexpr (variables/rename (build-node ['[clojure.string :as str]
                                                                 '[a.namespace :refer :all]] ; we don't actually know what is in a.namespace. could be lower-case symbol
                                                                '(str/lower-case "prueba"))
                                                    'clojure.string/lower-case
                                                    'lower-case
                                                    '[other.namespace :refer [lower-case]]))))))

(deftest handling-duplicate-require
  (testing "when updating a require and it is already present multiple times"
    (is (match? (build-code ['[clojure.string :as str]
                             '[clojure.string :as string]
                             'clojure.string]
                            '(do (str/lower-case "a")
                                 (str/lower-case "c")
                                 (str/lower-case "d")))
                (n/sexpr (variables/rename (build-node ['[clojure.string :as str :refer [upper-case]]
                                                        '[clojure.string :as string :refer [upper-case]]
                                                        '[clojure.string :refer [upper-case]]]
                                                       '(do (string/upper-case "a")
                                                            (upper-case "c")
                                                            (str/upper-case "d")))
                                           'clojure.string/upper-case
                                           'lower-case
                                           '[clojure.string :as string])))))

  (testing "renames that don't alter the name/ns work in presence of multiple duplicate imports"
    (is (match? (build-code ['[clojure.string :as str :refer [upper-case]]
                             'clojure.string]
                            '(do (upper-case "c")
                                 (upper-case "d")))
                (n/sexpr (variables/rename (build-node ['[clojure.string :as str :refer [upper-case]]
                                                        '[clojure.string :refer [upper-case]]]
                                                       '(do (upper-case "c")
                                                            (str/upper-case "d")))
                                           'clojure.string/upper-case
                                           'upper-case
                                           '[clojure.string :refer [upper-case]])))))

  (testing "when adding a require and old require present thrice"
    (is (match? (build-code ['[clojure.string :as str]
                             '[other.namespace :as other]
                             '[clojure.string :as string]
                             '[clojure.string :refer [lower-case]]]
                            '(do (other/another-case "a")
                                 (other/another-case "c")
                                 (other/another-case "d")))
                (n/sexpr (variables/rename (build-node ['[clojure.string :as str :refer [upper-case]]
                                                        '[clojure.string :as string :refer [upper-case]]
                                                        '[clojure.string :refer [lower-case upper-case]]]
                                                       '(do (string/upper-case "a")
                                                            (upper-case "c")
                                                            (str/upper-case "d")))
                                           'clojure.string/upper-case
                                           'another-case
                                           '[other.namespace :as other]))))))

(deftest whole-file-test
  (let [nodes    (p/parse-string-all (slurp "./resources/test/variables/before_rename.clj.fixture"))
        expected (slurp "./resources/test/variables/after_rename.clj.fixture")
        result   (variables/rename
                  nodes
                  'common-core.test-helpers/config
                  'config
                  '[common-test.config :as test.config])]
    (is (= expected
           (-> result z/edn z/root-string)))))

