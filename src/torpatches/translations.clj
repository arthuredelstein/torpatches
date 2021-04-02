(ns torpatches.translations
  "Utility functions"
  {:author "Arthur Edelstein"}
  (:require
   [torpatches.transifex :as transifex]))


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

