(ns torpatches.transifex
  "Use transifex"
  {:author "Arthur Edelstein"}
  (:require
   [clojure.string :as string]
   [torpatches.utils :as utils]
   [clj-http.client :as client]))

(def api-token
  "Returns the Transfex API token stored in ~/.transifex.
  Memoized so can be called repeatedly."
  (memoize
   (fn []
     (let [path (str (System/getProperty "user.home") "/.transifex")]
       (string/trim (slurp path))))))

(defn request
  "Make a request to the Transifex REST API. Returns JSON data."
  [url]
  (-> url
      (client/get {:basic-auth ["api" (api-token)]
                   :as :json})
      :body))

(defn resources
  "Get data on transifex resources."
  []
  (request "https://api.transifex.com/organizations/otf/projects/torproject/resources/"))

(defn statistics
  "Get statistics on a transifex resource."
  [project resource]
  (request (str "https://www.transifex.com/api/2/project/" project "/resource/" resource "/stats/")))

(defn report
  "Get report on translation activity."
  [project]
  (request (str "https://api.transifex.com/organizations/otf/reports/activity/?from_date=2017-01-01&to_date=2017-12-31&project_slug=" project)))

