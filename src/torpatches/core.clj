(ns torpatches.core
  "Program to generate the torpat.ch website. URLs like
   https://torpat.ch/5856 redirect or link to diff(s) on
   https://gitweb.torproject.org/ ."
  {:author "Arthur Edelstein"}
  (:require [torpatches.translations :as translations]
            [torpatches.transifex :as transifex]
            [torpatches.patches :as patches]
            [torpatches.patch-table :as patch-table]
            [torpatches.utils :as utils]
            [torpatches.html :as html]
            [hiccup.page :as page]
            ))

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
       [:li [:a {:href "/community-locales"} "Community Portal locales monitor"]]
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
        uplift-table-long (patch-table/uplift-table uplift-data true)
        uplift-table-short (patch-table/uplift-table uplift-data false)
        [single-patch-bugs multi-patch-bugs] (patches/singles-and-multiples bugs-list)]
    (patch-table/write-redirect-file single-patch-bugs)
    (println "Wrote redirects file.")
    (dorun (map patch-table/write-indirect-page multi-patch-bugs))
    (println "Wrote multipatch link files.")
    (write-index  "../../torpat.ch/index.html" short-branch uplift-table-long)
    (println "Wrote index.")
    (write-index  "../../torpat.ch/short" short-branch uplift-table-short)
    (println "Wrote short.")
    (translations/write-translations-pages)
    ))
