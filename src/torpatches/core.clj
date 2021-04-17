(ns torpatches.core
  "Program to generate the torpat.ch website. URLs like
   https://torpat.ch/5856 redirect or link to diff(s) on
   https://gitweb.torproject.org/ ."
  {:author "Arthur Edelstein"}
  (:require [torpatches.translations :as translations]
            [torpatches.transifex :as transifex]
            [torpatches.patches :as patches]
            [torpatches.utils :as utils]
            [torpatches.html :as html]
            [clojure.java.shell :as shell]
            [clojure.string :as string]
            [clojure.data.csv :as csv]
            [clojure.data.json :as json]
            [hiccup.page :as page]
            [hiccup.util]
            [clj-http.client :as client]))

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
             [:td.hash [:a {:href (patches/patch-url hash)}
                        (hiccup.util/escape-html hash)]]
             [:td.title (hiccup.util/escape-html title)]
             [:td (bugzilla-list-html bugzilla)]])))]]))

(defn redirect-line
  "Takes a [trac-ticket [[hash message]]] pair and generates
   an nginx redirect line."
  [[trac-ticket [[hash _]]]]
  (str "location /" trac-ticket " { rewrite ^ " (patches/patch-url hash) "; }\n"))

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
      [:li hash " " [:a {:href (patches/patch-url hash)}
                     (hiccup.util/escape-html message)]])]])

(defn write-patch-list-page
  "Create an HTML page that displays a list of links to patches
   given in commits."
  [tag title commits]
  (spit
   (str "../../torpat.ch/" tag)
   (page/html5
    (html/head title nil)
    [:body
     [:h3 title]
     (html-patch-list commits)
     (html/footer)])))

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
  (let [isolation-commits (filter #(-> % second (utils/contains-any ["solat" "#5742" "13900"]))
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
    (html/head "torpat.ch" "main.css")
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
       [:li [:a {:href "/tpo-locales"} "torproject.org locales monitor"]]
       [:li [:a {:href "/support-locales"} "Support Portal locales monitor"]]
       [:li [:a {:href "/manual-locales"} "Tor Browser User Manual locales monitor"]]
       [:li [:a {:href "https://arthuredelstein.net/exits"} "Tor Exit DNS Timeouts"]]]]
     [:h3 "Tor Browser Uplift Tracker"]
     [:p "Current tor-browser.git branch: "
      [:a {:href (str "https://gitweb.torproject.org/tor-browser.git/log/?h="
                      branch)} branch]]
     legend-table
     uplift-table
     (html/footer)
     ])))

(defn -main [& args]
  "The main program. Works out the Tor Browser trac ticket number for each
   patch. For bugs with a single patch, generates a redirect from
   https://torpat.ch/#### (where #### is the ticker number) to the patch at
   https://gitweb.torproject.org. For bugs with multiple patches,
   creates a page at https://torpat.ch/#### that links to each of those patches."
  (utils/fetch-latest-branches! "../tor-browser")
  (let [branch (patches/newest-tor-browser-branch)
        short-branch (last (.split branch "/"))
        bugs-list (patches/read-bugs-list branch)
        uplift-data (patches/uplift-data bugs-list)
        uplift-table-long (uplift-table uplift-data true)
        uplift-table-short (uplift-table uplift-data false)
        [single-patch-bugs multi-patch-bugs] (patches/singles-and-multiples bugs-list)]
    (write-redirect-file single-patch-bugs)
    (println "Wrote redirects file.")
    (dorun (map write-indirect-page multi-patch-bugs))
    (println "Wrote multipatch link files.")
    (write-index  "../../torpat.ch/index.html" short-branch uplift-table-long)
    (println "Wrote index.")
    (write-index  "../../torpat.ch/short" short-branch uplift-table-short)
    (println "Wrote short.")
    (translations/write-tbb-locale-page (translations/tbb-locale-data))
    (println "Wrote TBB locales page.")
    (translations/write-web-portal-locale-page
     {:path "../../torpat.ch/support-locales"
      :name "Tor Support Portal"
      :stats (transifex/statistics "tor-project-support-community-portal" "support-portal")})
    (translations/write-web-portal-locale-page
     {:path "../../torpat.ch/manual-locales"
      :name "Tor Browser User Manual"
      :stats (transifex/statistics "tor-project-support-community-portal" "tbmanual-contentspot")})
    (translations/write-web-portal-locale-page
     {:path "../../torpat.ch/tpo-locales"
      :name "torproject.org"
      :stats (transifex/statistics "tor-project-support-community-portal" "tpo-contentspot")})
    (println "Wrote portal locales pages.")))
