(defproject dev.nubank/umschreiben-cl "0.1.0"
  :description "Rewrite utilities for refactoring clojure files"

  :url "https://github.com/nubank/umschreiben-clj"

  :license {:name "The MIT License (MIT)"
            :url  "http://opensource.org/licenses/mit-license.php"}

  :repositories [["publish" {:url "https://clojars.org/repo"
                             :username :env/clojars_username
                             :password :env/clojars_passwd
                             :sign-releases false}]]

  :plugins [[lein-cljfmt "0.6.4" :exclusions [org.clojure/clojure]]
            [lein-nsorg "0.3.0" :exclusions [org.clojure/clojure]]
            [lein-ancient "0.6.14" :exclusions [commons-logging
                                                com.fasterxml.jackson.core/jackson-databind
                                                com.fasterxml.jackson.core/jackson-core]]]

  :dependencies [[org.clojure/clojure "1.10.3"]
                 [rewrite-clj "1.0.605-alpha"]]

  :profiles {:dev {:source-paths ["config"]
                   :dependencies [[org.clojure/tools.namespace "1.0.0"]
                                  [nubank/matcher-combinators "3.1.4"]]}}

  :aliases {"lint"     ["do" ["cljfmt" "check"] ["nsorg"]]
            "lint-fix" ["do" ["cljfmt" "fix"] ["nsorg" "--replace"]]}

  :repl-options {:init-ns user})
