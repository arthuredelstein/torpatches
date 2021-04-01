(defproject torpatches "0.1.0-SNAPSHOT"
  :description "Generate torpat.ch redirects and link pages."
  :url "https://torpat.ch"
  :main torpatches.core
  :license {:name "Public Domain"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/data.csv "1.0.0"]
                 [org.clojure/data.xml "0.0.8"]
                 [org.clojure/data.json "2.0.2"]
                 [reaver "0.1.3"]
                 [hiccup "1.0.5"]
                 [clj-http "2.3.0"]
                 [cheshire "5.10.0"]])
