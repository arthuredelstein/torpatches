(ns torpatches.html
  "Utilities for html to generate the torpat.ch website."
  {:author "Arthur Edelstein"}
  (:require [clojure.string :as string]))

(defn now-string
  "Returns the current date-time in UTC as a reasonably readable string."
  []
  (let [date-format (java.text.SimpleDateFormat. "yyyy-MMM-dd HH:mm 'UTC'")]
    (.setTimeZone date-format (java.util.TimeZone/getTimeZone "UTC"))
    (.format date-format (java.util.Date.))))

(defn compress-css
  [css-string]
  (string/replace css-string #"\s+" " "))

(defn embed-css
  [css-file]
  [:style {:type "text/css"} (compress-css (slurp css-file))])

(defn head
  [title css-file]
  [:head
   [:title title]
   [:meta {:charset "utf-8"}]
   (when css-file
     (embed-css css-file))])

(defn footer
  "A footer for each page."
  []
  [:p [:span {:style "font-style: italic"} "Last update: " (now-string) " "]
   [:span [:a {:href "https://github.com/arthuredelstein/torpatches"}
           "(Source on github)"]]])
