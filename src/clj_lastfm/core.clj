(ns clj-lastfm.core
  (:import (java.net URI)
           (java.text SimpleDateFormat)
           (java.util TimeZone))
  (:use [clj-lastfm.filecache]
        [clojure.contrib.json.read :only (read-json)]
        [clojure.walk :only (keywordize-keys)]
        [clojure.contrib.import-static]
        [clojure.contrib.logging]))

(import-static java.lang.Integer parseInt)
(import-static java.lang.Double parseDouble)

;;;;;;;;;; Basic ;;;;;;;;;;

(def #^{:private true}
  api-root-url ["http" "ws.audioscrobbler.com" "/2.0/"])

(defn- api-key []
  (let [lastfm-api-key (resolve '*lastfm-api-key*)]
    (if (nil? lastfm-api-key)
      (throw (IllegalStateException. "lastfm API key is not set"))
      lastfm-api-key)))

(def #^{:private true} guid-pattern
  #"^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$")

(def #^{:private true} sdf
  (doto (SimpleDateFormat. "EEE, dd MMMM yyyy HH:mm:ss +0000")
    (.setTimeZone (TimeZone/getTimeZone "GMT"))))

(defn- parse-date [date-str]
  (.parse sdf date-str))

(defn- remove-nil-values [m]
  (apply hash-map (apply concat (filter #(not (nil? (fnext %))) m))))

(defn- create-url-query [query-params]
  (apply str
    (interpose
      "&"
      (map
        #(str (name (first %)) "=" (second %))
        (remove-nil-values query-params)))))

(defn- create-url [query-params]
  (.toURL
    (apply
      #(URI. %1 %2 %3 %4 %5)
      (conj
        api-root-url
        (create-url-query
          (merge query-params {:api_key @(api-key) :format "json"}))
        nil))))

(def #^{:private true} default-cache (create-file-cache))

(defn- get-url
  ([url] (get-url url default-cache))
  ([url cache]
    (do
      (debug (str "URL: " url))
      (get-file-content cache url))))

(defn- get-data [params]
  (let [url (create-url params)]
    (-> url get-url read-json keywordize-keys)))

(defn- create-get-obj-fn [fixed-params parse-fn]
  (fn [more-params]
    (parse-fn (get-data (merge fixed-params more-params)))))

(defn- create-get-obj-field-fn [create-obj-fn extract-obj-id-fields-fn]
  (fn [obj field-kw]
    (let [field-val (obj field-kw)]
      (if (nil? field-val)
        ((apply create-obj-fn (extract-obj-id-fields-fn obj)) field-kw)
        field-val))))

;;;;;;;;;; forward declaration ;;;;;;;;;;

(declare bio-struct artist-struct tag-struct)

;;;;;;;;;; Bio/Wiki ;;;;;;;;;;

(defstruct bio-struct :published :summary :content)

(defn- parse-bio [data]
  (struct
    bio-struct
    (-> data :published parse-date)
    (-> data :summary)
    (-> data :content)))

;;;;;;;;;; artist.getinfo ;;;;;;;;;;

(defstruct artist-struct
  :name :url :mbid :streamable :listeners :playcount :bio)

(defn- parse-artist [data]
  (struct
    artist-struct
    (-> data :artist :name)
    (-> data :artist :url)
    (-> data :artist :mbid)
    (= 1 (-> data :artist :streamable parseInt))
    (-> data :artist :stats :listeners parseInt)
    (-> data :artist :stats :playcount parseInt)
    (-> data :artist :bio parse-bio)))

(def #^{:private true}
  get-artist (create-get-obj-fn {:method "artist.getinfo"} parse-artist))

(defmulti artist
  (fn [artist-or-mbid & _]
    (if (re-matches guid-pattern artist-or-mbid) :mbid :artist)))

(defmethod artist :artist
  ([artist-name] (artist artist-name nil nil))
  ([artist-name username] (artist artist-name username nil))
  ([artist-name username lang]
    (get-artist {:artist artist-name :username username :lang lang})))

(defmethod artist :mbid
  ([mbid] (artist mbid nil nil))
  ([mbid username] (artist mbid username nil))
  ([mbid username lang]
    (get-artist {:mbid mbid :username username :lang lang})))

(def artist-info
  (create-get-obj-field-fn artist #(vector (% :name))))

;;;;;;;;;; artist.getsimilar ;;;;;;;;;;

(defn- parse-artist-similar [data]
  (vec
    (map
      #(struct-map artist-struct
        :name (% :name)
        :url (% :url)
        :mbid (% :mbid)
        :streamable (= 1 (-> % :streamable parseInt))
        :match (-> % :match parseDouble))
      (-> data :similarartists :artist))))

(def #^{:private true} get-artist-similar
  (create-get-obj-fn {:method "artist.getsimilar"} parse-artist-similar))

(defn- artist-or-name [artst-or-name & _]
    (if (instance? clojure.lang.PersistentStructMap artst-or-name)
      :artist :name))

(defmulti artist-similar artist-or-name)

(defmethod artist-similar :artist
  ([artst] (-> artst :name artist-similar))
  ([artst limit] (artist-similar (artst :name) limit)))

(defmethod artist-similar :name
  ([artist-name]
    (get-artist-similar {:artist artist-name}))
  ([artist-name limit]
    (get-artist-similar {:artist artist-name :limit limit})))

;;;;;;;;;; artist.gettoptags ;;;;;;;;;;

(defn- parse-artist-toptags [data]
  (vec (map #(struct tag-struct (% :name) (% :url)) (-> data :toptags :tag))))

(def #^{:private true} get-artist-toptags
  (create-get-obj-fn {:method "artist.gettoptags"} parse-artist-toptags))

(defmulti artist-toptags artist-or-name)

(defmethod artist-toptags :artist [artst]
  (-> artst :name artist-toptags))

(defmethod artist-toptags :name [artist-name]
  (get-artist-toptags {:artist artist-name}))

;;;;;;;;;; tag ;;;;;;;;;;

(defstruct tag-struct :name :url)

(comment

(def *lastfm-api-key* "23caa86333d2cb2055fa82129802780a")
(def u2 (artist "u2"))
(println (artist-info u2 :url))

)