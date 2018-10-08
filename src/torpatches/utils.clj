(ns torpatches.utils
  "Utility functions"
  {:author "Arthur Edelstein"}
  (:require
   [clojure.string :as string]
   [clj-http.headers :as headers]
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

(defn de-key
  "Flattens a map of maps, so the outer map's keys
   become an item in the inner maps keyed to 'key'."
  [map key]
  (for [[outer-key data] map]
    (assoc data key (name outer-key))))

(defn maps-to-table-rows
  "Takes a list of maps and converts to table rows
  (a list of lists)."
  [header-items data]
  (for [datum data]
    (for [header-item header-items]
      (get datum header-item))))

(defn table-rows-to-html
  [header-items class-name rows]
  [:table {:class class-name}
   [:tr.header
    (for [header-item header-items]
      [:th (name header-item)])]
   (for [row rows]
     [:tr
      (for [item row]
        [:td item])])])
