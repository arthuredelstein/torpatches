(ns torpatches.patches
  "Functions to obtain data about Tor Browser patch uplift status."
  {:author "Arthur Edelstein"}
  (:require [torpatches.utils :as utils]
            [clojure.java.shell :as shell]
            [clojure.string :as string]
            [clojure.data.csv :as csv]
            [clojure.data.json :as json]
            [clj-http.client :as client]))

(defn newest-tor-browser-branch
  "Get the name of the most recent Tor Browser alpha branch.
   Assumes branches are named by semantic versioning."
  []
  (->> (utils/branches "../tor-browser")
       (map #(re-find #"remotes/origin/tor-browser-(.*?)esr-(.*?)-.*?$" %))
       (map reverse)
       (map vec)
       sort
       last
       last))

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

(defn remove-mozilla-commits
  "Remove mozilla commits, which are obvious from an 'r=' tag
   or similar."
  [commits]
  (remove #(let [[hash msg] %]
             (utils/contains-any msg
                                 ["r=" "a=" "No bug," "CLOSED TREE" "Backed out changeset" "1289001) for bustage"]))
          commits))

(defn read-bugs-list
  "Retrieve a list of [commit-hash commit-message]."
  [branch]
  (->> (utils/latest-commits "../tor-browser" branch 300)
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
    (-> (utils/curl-json (str "https://bugzilla.mozilla.org/rest/bug/"
                              bug-id
                              "/attachment"))
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
  (-> (utils/curl-json
       "https://bugzilla.mozilla.org/rest/bug?include_fields=id,whiteboard,summary,status,resolution,priority&f1=status_whiteboard&f2=short_desc&j_top=OR&o1=anywordssubstr&o2=anywordssubstr&v1=%5Btor&v2=%5Btor%20(tor%20%5BTor%20(Tor")
      (get :bugs)))

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

(defn assemble-data-for-tor-commit
  "Combines data from Tor and Mozilla for a given tor patch."
  [tor-bug tor-to-mozilla-map trac-data]
  (let [[hash title] tor-bug
        id (bug-number title)
        bugzilla (tor-to-mozilla-map id)
        bugzilla2 (map #(utils/elucidate % :id fetch-mozilla-review-flags :flags) bugzilla)
     ;   bugzilla2 bugzilla
;        bugzilla2 (pmap #(utils/elucidate % :id fetch-hg-commits :hg) bugzilla)
        id-clean (cleanup-bug-number id)
;        trac (trac-data id-clean)
;        trac2 (utils/elucidate trac "keywords" extract-keywords "keywords")
        ]
        {:hash hash :title title :id id :bugzilla bugzilla2
         ;:trac trac2
         }))

(defn uplift-data
  "Retrieves the full uplift table data given a list of tor patches."
  [tor-bugs-list]
  (let [mozilla-bugs (fetch-mozilla-bugs)
        mozilla-bug-map (mozilla-bugs-by-tor-id mozilla-bugs)
        ids (map (comp cleanup-bug-number bug-number second) tor-bugs-list)
                                        ;trac-data (fetch-trac-data ids)
        ]
    (for [tor-bug tor-bugs-list]
      (assemble-data-for-tor-commit tor-bug mozilla-bug-map nil)))); trac-data))))

(defn singles-and-multiples
  "Takes the bug list and returns a vector pair of two bugs maps--
   the first has bugs with single patches, the second
   bugs with multiple patches."
  [bug-list]
  (->> bug-list
       (group-by #(-> % second bug-number))
       (utils/separate #(let [[k v] %] (= 1 (count v))))))

(defn patch-url
  "Generates a Tor Browser patch url from the patch hash."
  [hash]
  (str "https://gitweb.torproject.org/tor-browser.git/patch/?id=" hash))

