(defproject leftins "0.1.0-SNAPSHOT"
  :description "ClojureScript routines for manipulating 'left infinite' numbers"
  :url "https://github.com/loki3/leftins"

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2227"]
                 [org.clojure/core.async "0.1.303.0-886421-alpha"]]

  :plugins [[lein-cljsbuild "1.0.1"]]

  :source-paths ["src"]

  :cljsbuild {
    :builds [{:id "leftins"
              :source-paths ["src"]
              :compiler {
                :output-to "leftins.js"
                :output-dir "out"
                :optimizations :none
                :source-map true}}]})


