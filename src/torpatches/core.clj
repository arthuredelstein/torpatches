(ns torpatches.core
  "Program to generate the torpat.ch website. URLs like
   https://torpat.ch/5856 redirect or link to diff(s) on
   https://gitweb.torproject.org/ ."
  {:author "Arthur Edelstein"}
  (:require [clojure.java.shell :as shell]
            [clojure.string :as string]
            [clojure.data.csv :as csv]
            [hiccup.page :as page]
            [hiccup.util]
            [clj-http.client :as client]))

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

(defn branches
  "List names of git branches."
  [dir]
  (shell-lines "git branch -a" :dir dir))

(defn fetch-latest-branches! []
  "Download the latest tor-browser branches from git.torproject.org."
  (shell-lines "git fetch origin"
               :dir (.getCanonicalPath
                     (clojure.java.io/file "../tor-browser"))))


(defn newest-tor-browser-branch
  "Get the name of the most recent Tor Browser alpha branch.
   Assumes branches are named by semantic versioning."
  []
  (->> (branches "../tor-browser")
       (map #(re-find #"remotes/origin/tor-browser-(.*?)esr-(.*?)-.*?$" %))
       (map reverse)
       (map vec)
       sort
       last
       last))

(defn latest-commits
  "Get the latest n patches for the given branch."
  [branch n]
  (->> (shell-lines (str "git log --oneline "
                         branch "~" n ".." branch)
                    :dir "../tor-browser")
       (map #(string/split % #"\s" 2))))

(defn bug-number
  "Takes a commit message and extracts the bug number."
  [commit-message]
  (or (match #"(TB\d+)" commit-message)
      (match #"[Bb]ug \#?([0-9\.]+)" commit-message)
      (match #"\#?([0-9]+)" commit-message)
      "None"))

(defn cleanup-bug-number
  "Takes a bug-number and returns one compatible with trac"
  [bug-number]
  (match #"^([0-9]+)" bug-number))

(defn patch-url
  "Returns a URL for a tor-browser patch, given the hash."
  [hash]
  (str "https://gitweb.torproject.org/tor-browser.git/patch/?id=" hash))

(defn contains-any
  "Returns first item in fragments can that be found in string."
  [string fragments]
  (some #(.contains string %) fragments))

(defn remove-mozilla-commits
  "Remove mozilla commits, which are obvious from an 'r=' tag
   or similar."
  [commits]
  (remove #(let [[hash msg] %]
             (contains-any msg ["r=" "a=" "No bug,"]))
          commits))

(defn read-bugs-list
  "Retrieve a list of [commit-hash commit-message]."
  [branch]
  (->> (latest-commits branch 200)
       remove-mozilla-commits
       (remove nil?)))

(defn fetch-trac-tickets
  "Retrieve ticket data for a list of ticket ids"
  [ids headers]
  (let [id-clause (->> ids (map #(str "id=" %)) (string/join "&or&"))
        col-clause (->> headers (map #(str "col=" %)) (string/join "&"))
        url (str "https://trac.torproject.org/projects/tor/query?" id-clause "&" col-clause "&format=csv")]
    (->> url client/get :body csv/read-csv)))

(defn fetch-trac-data
  "Retrieve ticket data from trac for the given ticket ids. Returns a map of ids
   to record maps."
  [ids]
  (let [headers ["id" "summary" "keywords" "status"]]
    (->> (fetch-trac-tickets ids headers)
         rest
         (map #(zipmap headers %))
         (map #(vector (% "id") (dissoc % "id")))
         (into {}))))

(defn fetch-hg-commits
  "Fetches all mozilla-central commits for a given mozilla bug."
  [mozilla-bug-id]
  (let [url (str "https://hg.mozilla.org/mozilla-central/json-log?rev=Bug+"
                 mozilla-bug-id)]
    (-> (client/get url {:as :json})
        :body :entries)))

(defn fetch-mozilla-bugs
  "Retrieve whiteboard:[tor bugs from bugzilla.mozilla.org REST API"
  []
  (-> (client/get "https://bugzilla.mozilla.org/rest/bug?include_fields=id,whiteboard,summary,status,resolution,priority&f1=status_whiteboard&f2=short_desc&j_top=OR&o1=anywordssubstr&o2=anywordssubstr&v1=[tor&v2=[tor (tor [Tor (Tor"
      {:accept :json :as :json})
      :body :bugs))

(defn tor-bug-ids-from-mozilla-bug
  "Some bugzilla bugs have one or more Tor ticket labels in the summary or whiteboard.
   E.G.: 'Tests for first-party isolation of cache (Tor 13749)'
   Extract the label."
  [{:keys [summary whiteboard]}]
  (set
   (map second
        (concat (re-seq #"[\[\(][tT]or (.+?)[\)\],]" summary)
                (re-seq #"\[tor (.+?)\]" whiteboard)))))

(defn mozilla-bugs-by-tor-id
  "Group mozilla-bugs by the Tor ticket number we read from the summary"
  [mozilla-bugs]
  (->
   (group-by :tor
             (apply concat
                    (for [mozilla-bug mozilla-bugs]
                      (let [bugs (tor-bug-ids-from-mozilla-bug mozilla-bug)]
                        (for [bug bugs]
                          (assoc mozilla-bug :tor bug))))))
   (assoc "24052" #{{:id "1412081"}})))

(defn extract-keywords
  [keywords-string]
  (when keywords-string
    (sort-by #(.toLowerCase %)
             (string/split keywords-string #"[\,\s]+"))))

(defn elucidate
  "Get the value from data-map at key, applies (fetch-fn key) and inserts the
   return value back into data-map at name."
  [data-map key fetch-fn name]
  (assoc data-map name (fetch-fn (get data-map key))))

(defn assemble-data-for-tor-commit
  "Combines data from Tor and Mozilla for a given tor patch."
  [tor-bug tor-to-mozilla-map trac-data]
  (let [[hash title] tor-bug
        id (bug-number title)
        bugzilla (tor-to-mozilla-map id)
        bugzilla2 bugzilla
;        bugzilla2 (pmap #(elucidate % :id fetch-hg-commits :hg) bugzilla)
        id-clean (cleanup-bug-number id)
        trac (trac-data id-clean)
        trac2 (elucidate trac "keywords" extract-keywords "keywords")]
    {:hash hash :title title :id id :bugzilla bugzilla2 :trac trac2}))

(defn uplift-data
  "Retrieves the full uplift table data given a list of tor patches."
  [tor-bugs-list]
  (let [mozilla-bugs (fetch-mozilla-bugs)
        mozilla-bug-map (mozilla-bugs-by-tor-id mozilla-bugs)
        ids (map (comp cleanup-bug-number bug-number second) tor-bugs-list)
        trac-data (fetch-trac-data ids)]
    (for [tor-bug tor-bugs-list]
      (assemble-data-for-tor-commit tor-bug mozilla-bug-map trac-data))))

(defn hg-patch-list-html
  "Generates some HTML to present a list of Mozilla mercurical patches
   that have landed for the given hg bug. Presents them between square
   brackets and separated by commas."
  [hg]
  (when (seq hg)
    [:span
     "&nbsp;["
     (interpose
      ",&nbsp;"
      (for [commit hg]
        (let [{:keys [node desc]} commit]
          [:a {:title (hiccup.util/escape-html desc)
               :href (str "https://hg.mozilla.org/mozilla-central/rev/"
                          node)}
           (.substring node 0 8)])))
     "]"]))

(defn bugzilla-fixed?
  [{:keys [status resolution]}]
  (or (= status "RESOLVED")
      (= resolution "FIXED")))

(defn bugzilla-list-html
  "Shows a list of bugzilla bugs and the corresponding Firefox patches, if any."
  [bugzilla]
  (for [bz-bug bugzilla]
    (let [{:keys [summary status resolution id hg priority]} bz-bug
          fixed (bugzilla-fixed? bz-bug)]
      [:p
       [:a
        {:class (if fixed "resolved" "unresolved")
         :title (hiccup.util/escape-html summary)
         :href (str "https://bugzilla.mozilla.org/" id)}
        id]
       (when (and (not fixed)
                  (not-empty priority)
                  (not= "--" priority))
         (str "(" priority ")"))
       (hg-patch-list-html hg)])))

(defn uplift-table
  "Generates the entire uplift table in HTML."
  [uplift-data]
  (do
    [:table.uplift
     [:tr.header
      [:th "Tor #"]
      [:th "Tor keywords"]
      [:th "Tor hash"]
      [:th "Tor name"]
      [:th "Moz # (Prio)"]
     ; [:th "Mozilla commits"]
      (for [{:keys [id title status hash trac bugzilla]} uplift-data]
        (let [resolved (apply = true (map bugzilla-fixed? bugzilla))
              state (cond (empty? bugzilla) "unfiled"
                          resolved "resolved"
                          (not resolved) "unresolved")]
          [:tr {:class state}
           [:td.id [:a {:href (str "https://trac.torproject.org/" id)
                        :title (hiccup.util/escape-html (get trac "summary"))}
                    (hiccup.util/escape-html id)]]
           [:td.keywords (for [keyword (get trac "keywords")]
                           [:p (hiccup.util/escape-html keyword)])]
           [:td.hash [:a {:href (patch-url hash)}
                      (hiccup.util/escape-html hash)]]
           [:td.title (hiccup.util/escape-html title)]
           [:td (bugzilla-list-html bugzilla)]]))]]))

(defn separate
  "Returns [coll-true coll-false], where coll-true is every
   member of coll that is true, and coll-false is every
   member of coll that is false."
  [pred coll]
  [(filter pred coll)
   (remove pred coll)])

(defn singles-and-multiples
  "Takes the bug list and returns a vector pair of two bugs maps--
   the first has bugs with single patches, the second
   bugs with multiple patches."
  [bug-list]
  (->> bug-list
       (group-by #(-> % second bug-number))
       (separate #(let [[k v] %] (= 1 (count v))))))

(defn patch-url
  "Generates a Tor Browser patch url from the patch hash."
  [hash]
  (str "https://gitweb.torproject.org/tor-browser.git/patch/?id=" hash))

(defn redirect-line
  "Takes a [trac-ticket [[hash message]]] pair and generates
   an nginx redirect line."
  [[trac-ticket [[hash _]]]]
  (str "location /" trac-ticket " { rewrite ^ " (patch-url hash) "; }\n"))

(defn now-string
  "Returns the current date-time in UTC as a reasonably readable string."
  []
  (let [date-format (java.text.SimpleDateFormat. "yyyy-MMM-dd HH:mm 'UTC'")]
    (.setTimeZone date-format (java.util.TimeZone/getTimeZone "UTC"))
    (.format date-format (java.util.Date.))))

(defn footer
  "A footer for each page."
  []
  [:p [:span {:style "font-style: italic"} "Last update: " (now-string) " "]
   [:span [:a {:href "https://github.com/arthuredelstein/torpatches"}
           "(Source on github)"]]])

(defn write-redirect-file
  "Create a redirect file from the single-patch bugs map."
  [single-patch-bugs]
  (spit
   "/etc/nginx/redirects.txt"
   (apply str (map redirect-line single-patch-bugs))))

(defn html-patch-list
  "Creates an HTML list of links to patches given in commits."
  [commits]
  [:pre
   [:ul
    (for [[hash message] commits]
      [:li hash " " [:a {:href (patch-url hash)} (hiccup.util/escape-html message)]])]])

(defn write-patch-list-page
  "Create an HTML page that displays a list of links to patches
   given in commits."
  [tag title commits]
  (spit
   (str "../../torpat.ch/" tag)
   (page/html5
    [:head [:title title] [:meta {:charset "utf-8"}]]
    [:body
     [:h3 title]
     (html-patch-list commits)
     (footer)])))

(defn write-uplift-page
  "Writes the uplift page, given the HTML uplift table."
  [branch uplift-table]
  (spit "../../torpat.ch/uplift"
        (page/html5
         [:head
          [:title "Tor Uplift Tracker"]
          [:meta {:charset "utf-8"}]
          (page/include-css "main.css")]
         [:body
          [:h3 "Tor Uplift Tracker"]
          [:p "Current tor-browser.git branch: "
           [:a {:href (str "https://gitweb.torproject.org/tor-browser.git/log/?h="
                           branch)} branch]]
          uplift-table
          (footer)])))

(defn write-indirect-page
  "Create an HTML page that displays a list of links to patches
   for a given Tor Browser bug."
  [[ticket commits]]
  (let [title (str "Patches for Tor Browser Bug #" ticket)]
    (write-patch-list-page ticket title commits)))

(defn write-isolation-page
  "Create an HTML page that displays a list of the isolation
   pages."
  [bugs-list]
  (let [isolation-commits (filter #(-> % second (contains-any ["solat" "#5742" "13900"]))
                                  bugs-list)]
    (write-patch-list-page "isolation"
                           "Tor Browser Isolation Patches"
                           isolation-commits)))

(defn write-index
  "Write an index.html file that is visible at https://torpat.ch .
   Shows time of last update."
  [branch bugs-list]
  (spit
   "../../torpat.ch/index.html"
   (page/html5
    [:head [:title "torpat.ch"] [:meta {:charset "utf-8"}]]
    [:body
     [:h3 "torpat.ch"]
     [:div "Useful links:"
      [:ul
       [:li "Current tor-browser.git branch: "
        [:a {:href (str "https://gitweb.torproject.org/tor-browser.git/log/?h="
                        branch)} branch]]
       [:li [:a {:href "https://bugzilla.mozilla.org/buglist.cgi?quicksearch=whiteboard%3A[tor"}
             "whiteboard:[tor bugs on bugzilla.mozilla.org"]]
       [:li [:a {:href "/isolation"} "Isolation patches"]]
       [:li [:a {:href "https://wiki.mozilla.org/Security/Tor_Uplift/Tracking"} "Mozilla's Tor patch uplift bug dashboard"]]
       [:li [:a {:href "https://wiki.mozilla.org/Security/FirstPartyIsolation"} "Mozilla's first-party isolation uplift patch dashboard"]]
       [:li [:a {:href "https://wiki.mozilla.org/Security/Fingerprinting"} "Mozilla's fingerprinting uplift patch dashboard"]]
       [:li [:a {:href "/uplift"} "Tor -> Mozilla bug concordance for tracking patch uplift (Under construction)"]]]]
     (comment [:div "Full list of tor-browser bugs:"
      (html-patch-list bugs-list)])
     (footer)
     ])))

(defn -main [& args]
  "The main program. Works out the Tor Browser trac ticket number for each
   patch. For bugs with a single patch, generates a redirect from
   https://torpat.ch/#### (where #### is the ticker number) to the patch at
   https://gitweb.torproject.org. For bugs with multiple patches,
   creates a page at https://torpat.ch/#### that links to each of those patches."
  (fetch-latest-branches!)
  (let [branch (newest-tor-browser-branch)
        short-branch (last (.split branch "/"))
        bugs-list (read-bugs-list branch)
        uplift-table (uplift-table (uplift-data bugs-list))
        [single-patch-bugs multi-patch-bugs] (singles-and-multiples bugs-list)]
    (write-redirect-file single-patch-bugs)
    (println "Wrote redirects file.")
    (write-isolation-page bugs-list)
    (println "Wrote isolation page.")
    (dorun (map write-indirect-page multi-patch-bugs))
    (println "Wrote multipatch link files.")
    (write-uplift-page short-branch uplift-table)
    (println "Wrote uplift page.")
    (write-index short-branch bugs-list)
    (println "Wrote index.")))
