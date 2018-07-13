(ns torpatches.utils
  "Utility functions"
  {:author "Arthur Edelstein"}
  (:require
   [clojure.string :as string]
   [clojure.java.shell :as shell]))

(defn match
  "Use a regular expression to find the first matching
   item (use parentheses)."
  [re s]
  (some-> (re-find re s) second))

(defn shell-lines
  "Send the line to the shell, and return a sequence
   of lines from the resulting process's output."
  [line & etc]
  (->> (apply shell/sh
              (concat (clojure.string/split line #" ")
                      etc))
       :out
       string/split-lines
       (map string/trim)))

(defn fetch-latest-branches!
  "Download the latest remote branches in a git repository."
  [repo-dir]
  (shell-lines "git fetch origin"
               :dir (.getCanonicalPath
                     (clojure.java.io/file repo-dir))))

