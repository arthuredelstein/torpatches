(ns torpatches.translations
  "Utility functions"
  {:author "Arthur Edelstein"}
  (:require
   [clj-http.client :as client]
   [clojure.string :as string]
   [hiccup.page :as page]
   [reaver]
   [torpatches.html :as html]
   [torpatches.transifex :as transifex]
   [torpatches.utils :as utils]
   ))


(def tbb-locale-resources
  [
   "abouttor-homepage"
   "browseronboardingproperties"
   "tor-browser-android-strings"
   "tor-launcher-network-settingsdtd"
   "tor-launcher-properties"
   "torbutton-aboutdialogdtd"
   "torbutton-abouttbupdatedtd"
   "torbutton-branddtd"
   "torbutton-brandproperties"
   "torbutton-torbuttondtd"
   "torbutton-torbuttonproperties"
;   "torbutton-securitylevelproperties"
   ])

(def counting-keys
  [:untranslated_words :translated_words
   :translated_entities :untranslated_entities
   :reviewed])

(defn analyze-translation-completeness
  "Figure out statistics per locale for TBB"
  []
  (let [raw-data (map #(transifex/statistics "torproject" %) tbb-locale-resources)
        locales (->> raw-data
                     (map keys)
                     (map set)
                     (apply clojure.set/intersection))]
    (for [locale locales]
      (let [locale-raw (map #(get % locale) raw-data)
            counts (map #(select-keys % counting-keys) locale-raw)
            sums (apply merge-with + counts)]
        (assoc sums :locale (name locale))))))

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

(defn normalize-locale [locale]
  (when locale
    (let [locale2 (.replace locale "-" "_")]
      (get {
            "bn_BD" "bn"
            "bn_IN" "bn"
            "en_US" "en"
            "es_ES" "es"
            "fy_NL" "fy"
            "ga_IE" "ga"
            "gu_IN" "gu"
            "hi_IN" "hi"
            "hr_HR" "hr"
            "hy_AM" "hy"
            "ko_KR" "ko"
            "ms_MY" "ms"
            "nb_NO" "nb"
            "ne_NP" "ne"
            "nn_NO" "nn"
            "pa_IN" "pa"
            "pt_PT" "pt"
            "si_LK" "si"
            "sk_SK" "sk"
            "sl_SI" "sl"
            "sv_SE" "sv"
            "ur_PK" "ur"
            }
           locale2 locale2))))

(defn firefox-locales
  "Gets a list of deployed Firefox locales from Mozilla."
  []
  (let [url "https://www.mozilla.org/en-US/firefox/all/"]
    (->> url client/get :body
         (re-seq #"lang\=\"(.+?)\"")
         (map second)
         (map normalize-locale)
         set
         sort)))

(defn current-tbb-alpha-locales
  []
  (->>
   (client/get "https://www.torproject.org/download/alpha/")
   :body
   (re-seq #"tor-browser-linux64.*?a.*?_(.*?)\.tar.xz[^.]")
   (map second)
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

(defn locale-names []
  (let [page (slurp "https://ss64.com/locale.html")
        data (reaver/extract-from
              (reaver/parse page) "#localetbl tr"
              [:item]
              "td" reaver/text)]
    (->> data
         (map :item)
         (map #(vector (normalize-locale (second %))
                       (first %)))
         (into {}))))

(defn tbb-locale-data
  []
  (let [locale-names (locale-names)
        current (current-tbb-alpha-locales)
        firefox (map normalize-locale (firefox-locales))
        gb-total (/ (disk-space-bytes nil) 1073741824)
        gb-single (/ (disk-space-bytes "zh-CN")
                        1073741824)
        progress (analyze-translation-completeness)
        progress+ (for [row progress]
                    (let [locale (normalize-locale (:locale row))]
                      (-> row
                          (assoc :locale_name
                                 (locale-names locale))
                          (assoc :firefox
                                 (if ((set firefox) locale)
                                   "yes" "no"))
                          (assoc :tbb_deployed
                                 (if ((set (map normalize-locale current)) locale)
                                   "yes" "no")))))]
    {:resources tbb-locale-resources
     :current current
     :gb-total gb-total
     :gb-single gb-single
     :progress progress+}))

(defn tbb-locale-table
  [data]
  (let [headers [:locale :locale_name :tbb_deployed :firefox :translated_entities :untranslated_entities :reviewed :translated_words :untranslated_words]]
    (->> data
         (sort-by :locale)
         reverse
         (sort-by :deployed)
         (sort-by :translated_entities)
         reverse
         (utils/maps-to-table-rows headers)
         (utils/table-rows-to-html headers "locale"))))

(defn write-tbb-locale-page
  [{:keys [translated current new gb-total gb-single gb-new progress resources]}]
  (spit
   "../../torpat.ch/locales"
   (page/html5
    (html/head "torpat.ch: Tor Browser locales" "locale.css")
    [:body
     [:h2 "Monitoring Tor Browser locales"]
     [:p.label "Tor Browser alphas already deployed:"]
     [:p (clojure.string/join ", " current)]
     [:p.label "Total occupied Tor Browser disk space:"]
     [:p (format "%.2f" gb-total) " GB"]
     [:p.label "Needed disk space for one locale:"]
     [:p (format "%.2f" gb-single) " GB"]
     [:p.label "String files required for Tor Browser:"]
     [:p (interpose [:br] resources)]
     [:p.label "Translation progress:"]
     [:p (tbb-locale-table progress)]
     (html/footer)])))

(defn web-portal-data
  [stats]
  (->> (utils/de-key stats :language)
       (sort-by :language)
       reverse
       (sort-by :translated_entities)
       reverse))

(defn web-portal-table
  [data]
  (let [headers [:language :completed :translated_entities :reviewed_percentage :last_commiter :last_update]]
    (->> data
         (utils/maps-to-table-rows headers)
         (utils/table-rows-to-html headers "locale"))))

(def tier-1-languages
  (set ["en" "fa" "es" "ru" "zh_CN" "pt_BR" "fr" "de" "ko" "tr" "it" "ar"]))

(defn write-web-portal-locale-page
  [{:keys [stats name path]}]
  (let [all (web-portal-data stats)
        tier-1 (filter #(-> % :language tier-1-languages) all)]
    (spit
     path
     (page/html5
      (html/head
       (str "torpat.ch: Monitoring " name " locales")
       "locale.css")
      [:body
       [:h1 (str "Monitoring " name " locales")]
       [:p.label "Translation progress (Tier 1 locales):"]
       (web-portal-table tier-1)
       [:p.label "Translation progress (all locales):"]
       (web-portal-table all)
       (html/footer)]))))
