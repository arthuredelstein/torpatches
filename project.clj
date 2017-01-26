(defproject torpatches "0.1.0-SNAPSHOT"
  :description "Generate torpat.ch redirects and link pages."
  :url "https://torpat.ch"
  :main torpatches.core
  :license {:name "Public Domain"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/data.csv "0.1.3"]
                 [hiccup "1.0.5"]
                 [clj-http "2.2.0"]
                 [cheshire "5.6.3"]])
