(ns torpatches.core
  (:require [clojure.java.shell :as shell]
            [hiccup.page :as page]))

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
  (shell-lines "git fetch origin" :dir "../tor-browser")
  (let [branch (newest-tor-browser-branch)]
    (->> (shell-lines (str "git log --oneline "
                           branch "~" n ".." branch)
                      :dir "../tor-browser")
         (map #(clojure.string/split % #"\s" 2)))))

(defn match
  "Use a regular expression to find the first matching
   item (use parentheses)."
  [re s]
  (some-> (re-find re s) second))

(defn bug-number
  "Takes a commit message and extracts the bug number."
  [commit-message]
  (or (match #"(TB\d+)" commit-message)
      (match #"[Bb]ug \#?([0-9]+)" commit-message)
      (match #"\#?([0-9]+)" commit-message)))

(defn patch-url
  "Returns a URL for a tor-browser patch, given the hash."
  [hash]
  (str "https://gitweb.torproject.org/tor-browser.git/patch/?id=" hash))

(defn remove-mozilla-commits
  "Remove mozilla commits, which are obvious from an 'r=' tag."
  [commits]
  (remove #(let [[hash msg] %] (.contains msg "r=")) commits))

(defn bugs-map
  "A map of trac.torproject.org ticket numbers to
   a vector of [commit-hash commit-message]."
  []
  (->
   (->> (latest-commits 200)
        remove-mozilla-commits
        (group-by #(-> % second bug-number)))
   (dissoc nil)))

(defn separate
  "Returns [coll-true coll-false], where coll-true is every
   member of coll that is true, and coll-false is every
   member of coll that is false."
  [pred coll]
  [(filter pred coll)
   (remove pred coll)])

(defn singles-and-multiples
  "Takes the bug map and returns a vector pair of two bugs maps--
   the first has bugs with single patches, the second
   bugs with multiple patches."
  [bug-map]
  (separate #(let [[k v] %] (= 1 (count v))) bug-map))

(defn patch-url
  "Generates a Tor Browser patch url from the patch hash."
  [hash]
  (str "https://gitweb.torproject.org/tor-browser.git/patch/?id=" hash))

(defn redirect-line
  "Takes a [trac-ticket [[hash message]]] pair and generates
   an nginx redirect line."
  [[trac-ticket [[hash _]]]]
  (str "location /" trac-ticket " { rewrite ^ " (patch-url hash) "; }\n"))

(defn write-redirect-file
  "Create a redirect file from the single-patch bugs map."
  [single-patch-bugs]
  (spit
   "/etc/nginx/redirects.txt"
   (apply str (map redirect-line single-patch-bugs))))

(defn write-indirect-page
  "Create an HTML page that displays a list of links to patches
   for a given Tor Browser bug."
  [[ticket commits]]
  (spit
   (str "../../torpat.ch/" ticket)
   (let [title (str "Patches for Tor Browser Bug #" ticket)]
     (page/html5
      [:head [:title title]]
      [:body
       [:h3 title]
       [:pre
        [:ul
         (for [[hash message] commits]
           [:li hash " " [:a {:href (patch-url hash)} message]])]]]))))

(defn write-index
  []
  (spit
   "../../torpat.ch/index.html"
   (page/html5
    [:head [:title "torpat.ch"]]
    [:body
     [:h3 "torpat.ch"]
     [:p "Last update: " (.toString (java.util.Date.))]])))

(defn -main [& args]
  "The main program. Works out the Tor Browser trac ticket number for each
   patch. For bugs with a single patch, generates a redirect from
   https://torpat.ch/#### (where #### is the ticker number) to the patch at
   https://gitweb.torproject.org. For bugs with multiple patches,
   creates a page at https://torpat.ch/#### that links to each of those patches."
  (let [[single-patch-bugs multi-patch-bugs] (singles-and-multiples (bugs-map))]
    (write-redirect-file single-patch-bugs)
    (println "Wrote redirects file.")
    (dorun (map write-indirect-page multi-patch-bugs))
    (println "Wrote multipatch link files.")
    (write-index)
    (println "Wote index."))
