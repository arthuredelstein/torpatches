(ns torpatches.patch-table
  "Functions to generate the patch table pages."
  {:author "Arthur Edelstein"}
  (:require
   [torpatches.patches :as patches]
   [torpatches.html :as html]
   [clojure.string :as string]
   [hiccup.page :as page]
   [hiccup.util]
   ))

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
