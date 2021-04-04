(ns torpatches.utils
  "Utility functions"
  {:author "Arthur Edelstein"}
  (:require
   [clojure.string :as string]
   [clj-http.headers :as headers]
   [clojure.data.json :as json]
   [clojure.java.shell :as shell]))

(defn match
  "Use a regular expression to find the first matching
   item (use parentheses)."
  [re s]
  (some-> (re-find re s) second))

(defn contains-any
  "Returns first item in fragments can that be found in string."
  [string fragments]
  (some #(.contains string %) fragments))

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

(defn branches
  "List names of git branches."
  [dir]
  (shell-lines "git branch -a" :dir dir))

(defn latest-commits
  "Get the latest n patches for the given git branch."
  [dir branch n]
  (->> (shell-lines (str "git log --oneline "
                               branch "~" n ".." branch)
                          :dir dir)
       (map #(string/split % #"\s" 2))))

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

(defn curl
  [url]
  (-> (shell/sh "/usr/bin/curl" url) :out))

(defn curl-json
  [url]
  (-> url curl (json/read-str :key-fn keyword)))

(defn elucidate
  "Get the value from data-map at key, applies (fetch-fn key) and inserts the
   return value back into data-map at name."
  [data-map key fetch-fn name]
  (assoc data-map name (fetch-fn (get data-map key))))

(defn separate
  "Returns [coll-true coll-false], where coll-true is every
   member of coll that is true, and coll-false is every
   member of coll that is false."
  [pred coll]
  [(filter pred coll)
   (remove pred coll)])
