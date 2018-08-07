(ns torpatches.translations
  "Utility functions"
  {:author "Arthur Edelstein"}
  (:require
   [clojure.java.io :as io]
   [clojure.data.xml :as xml]
   [clojure.data.json :as json]
   [clojure.string :as string]
   [torpatches.utils :as utils]))

(def tbb-locale-branches
  [
   "tor-launcher-network-settings"
   "tor-launcher-properties"
   "tor-launcher-progress"
   "torbutton-aboutdialogdtd"
   "torbutton-abouttbupdatedtd"
   "torbutton-abouttorproperties"
   "torbutton-branddtd"
   "torbutton-brandproperties"
   "torbutton-torbuttondtd"
   "torbutton-torbuttonproperties"
  ]
)

(def support-locale-branches
  [
   "support-portal"
  ]
)

(defn checkout-branch! [repo-dir branch]
  (utils/shell-lines (str "git checkout " branch)
                     :dir (.getCanonicalPath
                           (clojure.java.io/file repo-dir))))

(defn files-of-type [repo-dir suffix]
  (->> (file-seq (io/as-file repo-dir))
       (filter #(-> % .getName (.endsWith (str "." suffix))))))

(defn parse-properties-file
  [properties-file]
  (->> (slurp properties-file)
       string/split-lines
       (remove string/blank?)
       (remove #(.startsWith % "#"))
       (map #(string/split % #"=" 2))
       (map #(mapv string/trim %))
       (into {})))

(defn parse-dtd-line [line]
;  (println "line: " line)
  (vec (next (re-find #"\<\!ENTITY\s+(\S+)\s+\"(.+)\"\s*\>" line))))

(defn parse-dtd-file
  [dtd-file]
  (->> (slurp dtd-file)
       string/split-lines
       (map parse-dtd-line)
       (filter #(= (count %) 2))
       (into {})))

(defn parse-json-file
  [json-file]
  (->> (slurp json-file)
       json/read-str
       ))

(defn file-locale [file]
  (-> file .getParentFile .getName))

(defn get-en-file [files]
  (->> files
       (filter #(= (file-locale %) "en"))
       first))

(defn count-localized-strings [en-map other-map]
  (count
   (filter identity
           (for [string-key (keys en-map)]
             (let [en-val (en-map string-key)
                   other-val (other-map string-key)]
               (and
                (not (string/blank? other-val))
                (not= en-val other-val)))))))


(defn measure-one-branch [name]
  (let [suffix (if (.startsWith name "support")
                 "json"
                 (condp #(.endsWith %2 %1) name
                   "properties" "properties"
                   "dtd" "dtd"
                   "tor-launcher-progress" "dtd"
                   "tor-launcher-network-settings" "dtd"
                   nil))]
    (checkout-branch! "../translation" (str "origin/" name))
    (let [files (files-of-type "../translation" suffix)
;          _ (println files)
          en-file (get-en-file files)
          parse (condp = suffix
                  "dtd" parse-dtd-file
                  "properties" parse-properties-file
                  "json" parse-json-file
                  nil)
          en-map (parse en-file)
          en-strings (vals en-map)
          en-word-count (->> en-strings
                             (mapcat #(clojure.string/split % #"\\n|\s"))
                             (remove empty?)
                             count)]
      {:en-word-count en-word-count
       :locale-progress
       (assoc
        (into {}
              (for [file files]
                (do ;(println file)
                  (let [other-map (parse file)]
                    [(file-locale file)
                     (count-localized-strings en-map other-map)]))))
        "en" (count en-map))
       })))

(defn measure-branches [branches]
  (into {}
        (for [branch branches]
          [branch (measure-one-branch branch)])))

(defn analyze-translation-completeness [branches]
  (let [data (vals (measure-branches branches))]
    {:locale-progress
     (->> (apply merge-with + (map :locale-progress data))
          (sort-by second)
          reverse)
     :en-word-count
     (apply + (map :en-word-count data))}))

