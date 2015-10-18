(ns torpatches.core
  (require [clojure.java.shell :as shell]))

(defn shell-lines
  "Send the line to the shell, and return a sequence
   of lines from the resulting process's output."
  [line & etc]
  (->> (apply shell/sh
              (concat (clojure.string/split line #" ")
                      etc))
       :out
       clojure.string/split-lines
       (map clojure.string/trim)))

(defn branches
  "List names of git branches."
  [dir]
  (shell-lines "git branch -a" :dir dir))

(defn newest-tor-browser-branch []
  "Get the name of the most recent Tor Browser branch.
   Assumes branches are named by semantic versioning."
  []
  (->> (branches "../tor-browser")
       (filter #(.startsWith % "remotes/origin/tor-browser-"))
       sort reverse first))

(defn latest-commits [n]
  "Get the latest n patches."
  []
  (let [branch (newest-tor-browser-branch)]
    (->> (shell-lines (str "git log --oneline "
                           branch "~" n ".." branch)
                      :dir "../tor-browser")
         (map #(clojure.string/split % #"\s" 2)))))

(defn bug-number
  "Takes a commit message and extracts the bug number."
  [commit-message]
  (some-> (re-find #"Bug \#?([0-9]+)" commit-message)
          second
          Integer/parseInt))

(defn patch-url
  "Returns a URL for a tor-browser patch, given the hash."
  [hash]
  (str "https://gitweb.torproject.org/tor-browser.git/patch/?id=" hash))
