(defproject day13 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :plugins [[lein-auto "0.1.3"]]
  :dependencies [[org.clojure/clojure "1.8.0"] [proto-repl "0.3.1"]]

  :profiles
  {:dev {:source-paths ["dev" "src" "test"]
         :dependencies [[org.clojure/tools.namespace "0.2.11"]]}})
