(ns torpatches.core
  "Program to generate the torpat.ch website. URLs like
   https://torpat.ch/5856 redirect or link to diff(s) on
   https://gitweb.torproject.org/ ."
  {:author "Arthur Edelstein"}
  (:require [torpatches.translations :as translations]
            [torpatches.transifex :as transifex]
            [torpatches.utils :as utils]
            [clojure.java.shell :as shell]
            [clojure.string :as string]
            [clojure.data.csv :as csv]
            [hiccup.page :as page]
            [hiccup.util]
            [hickory.core :as hickory]
            [hickory.select]
            [clj-http.client :as client]))

(defn branches
  "List names of git branches."
  [dir]
  (utils/shell-lines "git branch -a" :dir dir))

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
  (->> (utils/shell-lines (str "git log --oneline "
                               branch "~" n ".." branch)
                          :dir "../tor-browser")
       (map #(string/split % #"\s" 2))))

(defn bug-number
  "Takes a commit message and extracts the bug number."
  [commit-message]
  (or (utils/match #"(TB\d+)" commit-message)
      (utils/match #"[Bb]ug \#?([0-9\.]+)" commit-message)
      (utils/match #"\#?([0-9]+)" commit-message)
      (when (= commit-message "Allow std::unordered_*.") "24197")
      (when (= commit-message "Don't break accessibility support for Windows") "21240")
      (when (= commit-message "Revert \"Getting Tor Browser to build with accessibility enabled on Windows\"") "21240")
      (when (= commit-message "Getting Tor Browser to build with accessibility enabled on Windows") "21240")
      (when (= commit-message "We don't take the SANDBOX_EXPORTS path and fix compile issues along our way") "16010")
      "None"))

(defn cleanup-bug-number
  "Takes a bug-number and returns one compatible with trac"
  [bug-number]
  (utils/match #"^([0-9]+)" bug-number))

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
             (contains-any msg
                           ["r=" "a=" "No bug," "CLOSED TREE" "Backed out changeset" "1289001) for bustage"]))
          commits))

(defn read-bugs-list
  "Retrieve a list of [commit-hash commit-message]."
  [branch]
  (->> (latest-commits branch 300)
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

(defn fetch-mozilla-attachments
  [bug-id]
  (try
    (-> (client/get (str "https://bugzilla.mozilla.org/rest/bug/"
                         bug-id
                         "/attachment")
                    {:accept :json :as :json})
        :body
        :bugs
        (get (keyword (str bug-id))))
    (catch Exception e nil)))

(defn latest-review-status
  [flag-list]
  (->> flag-list
       (filter #(-> % :name (= "review")))
       last
       :status))

(defn fetch-mozilla-review-flags
  [bug-id]
  (->> (fetch-mozilla-attachments bug-id)
       (remove #(-> % :is_obsolete (= 1)))
       (map :flags)
       (map latest-review-status)
       (into (sorted-set))))

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
                      (let [tor-ids (tor-bug-ids-from-mozilla-bug mozilla-bug)]
                        (for [tor-id tor-ids]
                          (assoc mozilla-bug :tor tor-id))))))
   (assoc "24052" #{{:id "1412081"}})
   (assoc "24398" #{{:id "1412081"}})
;   (assoc "1344613" #{{:id "1344613"}})
   ))

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
        bugzilla2 (map #(elucidate % :id fetch-mozilla-review-flags :flags) bugzilla)
     ;   bugzilla2 bugzilla
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
  (or ;(= status "RESOLVED")
      (= resolution "FIXED")))

(defn bugzilla-list-html
  "Shows a list of bugzilla bugs and the corresponding Firefox patches, if any."
  [bugzilla]
  (for [bz-bug bugzilla]
    (let [{:keys [summary status resolution id hg priority flags]} bz-bug
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
         (str " (" priority ")")) " "
       (when-not (empty? flags)
         (str "["
              (string/join "," (map #(str "r" %) flags))
              "]"))
       (hg-patch-list-html hg)])))

(def legend-table
  "HTML table with color codes explained."
  [:div.key
   [:div.key-title "Key:"]
   [:div.resolved "Uplifted"]
   [:div.unresolved "Moz bug open"]
   [:div.no-uplift "Don't uplift"]
   [:div.unfiled "Untriaged"]
   [:div "[ "
    [:a {:href "/"} "long"] " | "
    [:a {:href "/short"} "short"]
    " ]"]])

(defn uplift-table
  "Generates the entire uplift table in HTML."
  [uplift-data show-completed]
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
              keywords (get trac "keywords")
              state (cond (or (some #{"tbb-no-uplift" "tbb-no-uplift-60"} keywords)
                              (.startsWith id "TB")
                              (= id "11641")
                              (= id "13252")
                              (.contains title "Omnibox: Add DDG")) "no-uplift"
                          (empty? bugzilla) "unfiled"
                          resolved "resolved"
                          (not resolved) "unresolved")]
          (when (or show-completed
                    (= state "unresolved")
                    (= state "unfiled"))
            [:tr {:class state}
             [:td.id [:a {:href (str "https://trac.torproject.org/" id)
                          :title (hiccup.util/escape-html (get trac "summary"))}
                      (hiccup.util/escape-html id)]]
             [:td.keywords (for [keyword keywords]
                             [:p (hiccup.util/escape-html keyword)])]
             [:td.hash [:a {:href (patch-url hash)}
                        (hiccup.util/escape-html hash)]]
             [:td.title (hiccup.util/escape-html title)]
             [:td (bugzilla-list-html bugzilla)]])))]]))

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

(defn compress-css
  [css-string]
  (clojure.string/replace css-string #"\s+" " "))

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
   (apply str "location /uplift { rewrite ^ / ; }\n"
          (map redirect-line single-patch-bugs))))

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
  [path branch uplift-table]
  (spit
   path
   (page/html5
    [:head
     [:title "torpat.ch"]
     [:meta {:charset "utf-8"}]
     [:style {:type "text/css"} (compress-css (slurp "main.css"))]]
    [:body
     [:h3 "torpat.ch"]
     [:div "Useful links:"
      [:ul
       [:li [:a {:href "https://bugzilla.mozilla.org/buglist.cgi?quicksearch=whiteboard%3A[tor"}
             "whiteboard:[tor bugs on bugzilla.mozilla.org"]]
       [:li [:a {:href "https://bugzilla.mozilla.org/buglist.cgi?priority=P1&f1=OP&f0=OP&o2=substring&f4=CP&query_format=advanced&j1=OR&f3=CP&f2=status_whiteboard&bug_status=UNCONFIRMED&bug_status=NEW&bug_status=ASSIGNED&bug_status=REOPENED&v2=%5Btor"}
             "P1 whiteboard:[tor bugs on bugzilla.mozilla.org"]]
       [:li [:a {:href "https://wiki.mozilla.org/Security/Tor_Uplift/Tracking"} "Mozilla's Tor patch uplift bug dashboard"]]
       [:li [:a {:href "https://wiki.mozilla.org/Security/FirstPartyIsolation"} "Mozilla's first-party isolation uplift patch dashboard"]]
       [:li [:a {:href "https://wiki.mozilla.org/Security/Fingerprinting"} "Mozilla's fingerprinting uplift patch dashboard"]]
       [:li [:a {:href "https://wiki.mozilla.org/Security/Fusion"} "Mozilla's Fusion page"]]
       [:li [:a {:href "/locales"} "Tor Browser locales monitor"]]
       [:li [:a {:href "/support-locales"} "Support Portal locales monitor"]]
       [:li [:a {:href "https://arthuredelstein.net/exits"} "Tor Exit DNS Timeouts"]]]]
     [:h3 "Tor Browser Uplift Tracker"]
     [:p "Current tor-browser.git branch: "
      [:a {:href (str "https://gitweb.torproject.org/tor-browser.git/log/?h="
                      branch)} branch]]
     legend-table
     uplift-table
     (footer)
     ])))

(defn completed-locales-in-branch
  [branch]
  (let [url (str "https://gitweb.torproject.org/translation.git/tree/?h="
                 branch
                 "_completed")]
    (->>
     (try (:body (client/get url))
          (catch Exception e ""))
     (re-seq #"translation.git/tree/(.*?)\?h\=")
     (map second)
     (remove #(or (= % "README")
                  (.startsWith % ".")
                  (empty? %)))
     (map #(string/replace % ".json" ""))
    )))

(defn completed-locales
  [locale-completed-branches]
  (->>
   (for [branch locale-completed-branches]
     (completed-locales-in-branch branch))
   (map set)
   (apply clojure.set/intersection)
   sort
   (map #(string/replace % "_" "-"))))

(defn current-tbb-alpha-locales
  []
  (->>
   (client/get "https://www.torproject.org/projects/torbrowser.html.en")
   :body
   (re-seq #"tor-browser-linux64.*?a.*?_(.*?)\.tar.xz[^.]")
   (map second)
   (map #(.replace % "es-ES" "es")) ; bit of a hack; oh well
   sort))

(defn tbb-locales-we-can-add
  [translated released]
  (sort (clojure.set/difference
         (set translated)
         (set released)
         #{"en" "sv"} ; redundant
         ;#{"en-GB" "fr-CA" "pt"} ; possibly redundant
         )))

(defn parse-file-size [s]
  (let [factor (cond (nil? s) 0
                     (.endsWith s "G") 1073741824
                     (.endsWith s "M") 1048576
                     (.endsWith s "K") 1024
                     :default 1)
        number-text (utils/match #"([0-9.]+)" s)]
    (* factor (Double/parseDouble number-text))))

(defn file-sizes [url locale]
  (let [lines (-> url client/get :body (string/split #"\n"))
        locale_token (when locale (str "_" locale "."))]
    (->> lines
         (filter #(if locale_token (.contains % locale_token) identity))
         (map #(utils/match #"\s([0-9.]+[KM]?)\s*?$" %))
         (remove nil?)
         (map parse-file-size)
         sort
         reverse)))

(defn dist-urls []
  (let [home "https://dist.torproject.org/torbrowser/"
        lines (-> home client/get :body (string/split #"\n"))]
    (->> lines
         (filter #(.contains % "folder.gif"))
;         (filter #(.contains % "8.0a"))
         (map #(utils/match #"href=\"(.*?)\"" %))
         (map #(str home %)))))

(defn disk-space-bytes
  "Looks at dist.torproject.org and works out how much disk
   space is taken up by a single locale (zh-CN for this case)."
  [locale]
  (let [pages (dist-urls)
        sizes (map #(file-sizes % locale) pages)]
    (->> sizes (apply concat) (apply +))))

(defn tbb-locale-data
  []
  (let [translated (completed-locales translations/tbb-locale-branches)
        current (current-tbb-alpha-locales)
        new (tbb-locales-we-can-add translated current)
        gb-total (/ (disk-space-bytes nil) 1073741824)
        gb-single (/ (disk-space-bytes "zh-CN")
                        1073741824)
        gb-new (* (count new) gb-single)
        completeness-data (translations/analyze-translation-completeness translations/tbb-locale-branches)]
    {:translated translated
     :current current
     :new new
     :gb-total gb-total
     :gb-single gb-single
     :gb-new gb-new
     :progress (completeness-data :locale-progress)
     :en-word-count (completeness-data :en-word-count)}))

(defn write-tbb-locale-page
  [{:keys [translated current new gb-total gb-single gb-new progress en-word-count]}]
  (spit
   "../../torpat.ch/locales"
   (page/html5
    [:head
     [:title "torpat.ch: Tor Browser locales"]
     [:style ".label { font-weight: bold; }"]]
    [:body
     [:h2 "Monitoring Tor Browser locales"]
;     [:p.label "Transifex translations complete:"]
;     [:p (clojure.string/join ", " translated)]
     [:p.label "Tor Browser alphas already deployed:"]
     [:p (clojure.string/join ", " current)]
;     [:p.label "Tor Browser locales 100% translated but not yet deployed:"]
;     [:p (clojure.string/join ", " new)]
     [:p.label "Total occuppied Tor Browser disk space:"]
     [:p (format "%.2f" gb-total) " GB"]
     [:p.label "Needed disk space for one locale:"]
     [:p (format "%.2f" gb-single) " GB"]
;     [:p.label "Expected additional disk space for all new locales:"]
                                        ;     [:p (format "%.2f" gb-new) " GB"]
     [:p.label "Number of source words to translate (from en-US):"]
     [:p (str en-word-count)]
     [:p.label "Translation progress (strings localized per locale):"]
     [:p [:table
          (let [current2 (->> current
                              (map #(.replace % "-" "_"))
                              (map #(.replace % "sv_SE" "sv"))
                              (map #(.replace % "en_US" "en")))]
            (for [[locale strings-count] progress]
              [:tr {:style (if ((set current2) locale)
                             "color: green")}
               [:td locale]
               [:td strings-count]]))]]
     (footer)])))

(defn support-portal-data
  [stats]
  (->> (utils/de-key stats :language)
       (sort-by :language)
       reverse
       (sort-by :translated_entities)
       reverse))

(defn support-portal-table
  [data]
  (let [headers [:language :completed :translated_entities :last_commiter :last_update]]
    (->> data
         (utils/maps-to-table-rows headers)
         (utils/table-rows-to-html headers "locale"))))

(def tier-1-languages
  (set ["en" "fa" "es" "ru" "zh_CN" "pt_BR" "fr" "de" "ko" "tr" "it" "ar"]))

(defn write-support-locale-page
  [{:keys [stats]}]
  (let [all (support-portal-data stats)
        tier-1 (filter #(-> % :language tier-1-languages) all)]
    (spit
     "../../torpat.ch/support-locales"
     (page/html5
      [:head
       [:title "torpat.ch: Support Portal locales"]
       [:style "table.locale tr:nth-child(odd) {background-color: #ddd;} table.locale { text-align: left; border-spacing: 0px; } table.locale tr th { background-color: #bbb; padding-right: 10px; } table.locale tr td { padding-right: 10px;} .label { font-weight: bold; font-size: larger;}"]]
      [:body
       [:h2 "Monitoring Support Portal locales"]
       [:p.label "Translation progress (Tier 1 locales):"]
       (support-portal-table tier-1)
       [:p.label "Translation progress (all locales):"]
       (support-portal-table all)
       [:p.label "Locales already deployed:"]
       [:p "TODO"]
                                        ;     [:p (clojure.string/join ", " current)]
       (footer)]))))

(defn -main [& args]
  "The main program. Works out the Tor Browser trac ticket number for each
   patch. For bugs with a single patch, generates a redirect from
   https://torpat.ch/#### (where #### is the ticker number) to the patch at
   https://gitweb.torproject.org. For bugs with multiple patches,
   creates a page at https://torpat.ch/#### that links to each of those patches."
  (utils/fetch-latest-branches! "../tor-browser")
  (let [branch (newest-tor-browser-branch)
        short-branch (last (.split branch "/"))
        bugs-list (read-bugs-list branch)
        uplift-data (uplift-data bugs-list)
        uplift-table-long (uplift-table uplift-data true)
        uplift-table-short (uplift-table uplift-data false)
        [single-patch-bugs multi-patch-bugs] (singles-and-multiples bugs-list)]
    (write-redirect-file single-patch-bugs)
    (println "Wrote redirects file.")
    (dorun (map write-indirect-page multi-patch-bugs))
    (println "Wrote multipatch link files.")
    (write-index  "../../torpat.ch/index.html" short-branch uplift-table-long)
    (println "Wrote index.")
    (write-index  "../../torpat.ch/short" short-branch uplift-table-short)
    (println "Wrote short.")
    (write-tbb-locale-page (tbb-locale-data))
    (println "Wrote TBB locales page.")
    (write-support-locale-page {:stats (transifex/statistics "support-portal")})
    (println "Wrote support locales page.")))
