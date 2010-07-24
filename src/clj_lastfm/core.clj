(ns clj-lastfm.core
  (:import (java.net URI)
           (java.text SimpleDateFormat)
           (java.util TimeZone))
  (:use [clj-lastfm.filecache]
        [clojure.contrib.json.read :only (read-json)]
        [clojure.walk :only (keywordize-keys)]
        [clojure.contrib.import-static]
        [clojure.contrib.logging]))

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

(def #^{:private true} attr-kw (keyword "@attr"))

(defn- safe-parse-int [n]
  (if (nil? n)
    nil
    (try
      (Integer/parseInt n)
      (catch NumberFormatException nfe nil))))

(defn- safe-parse-double [n]
  (if (nil? n)
    nil
    (try
      (Double/parseDouble n)
      (catch NumberFormatException nfe nil))))

(defn- struct? [obj] (instance? clojure.lang.PersistentStructMap obj))

(def #^{:private true :tag SimpleDateFormat} sdftz
  (doto (SimpleDateFormat. "EEE, dd MMMM yyyy HH:mm:ss +0000")
    (.setTimeZone (TimeZone/getTimeZone "GMT"))))

(def #^{:private true :tag SimpleDateFormat} sdf
  (doto (SimpleDateFormat. "EEE, dd MMMM yyyy HH:mm:ss")
    (.setTimeZone (TimeZone/getTimeZone "GMT"))))

(defn- parse-date [date-str]
  (try
    (.parse sdftz date-str)
    (catch java.text.ParseException e
      (.parse sdf date-str))))

(defn- remove-nil-values [m]
  (apply hash-map (apply concat (filter #(not (nil? (fnext %))) m))))

(defn- lastfm-url [path]
  (.toString (URI. "http" "www.last.fm" path nil nil)))

(defn- create-url-query [query-params]
  (apply str
    (interpose
      "&"
      (map
        #(str (name (first %)) "=" (second %))
        (remove-nil-values query-params)))))

(defn- create-url [query-params]
  (.toURL
    (#^URI apply
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
      (info (str "URL: " url))
      (get-file-content cache url))))

(defn- get-data [params]
  (let [url (create-url params)
        data (-> url get-url read-json keywordize-keys)]
    (if (-> data :error nil?)
      data
      (throw (IllegalArgumentException. (-> data :message str))))))

(defn- create-get-obj-fn [fixed-params parse-fn]
  (fn [more-params]
    (parse-fn #(get-data (merge fixed-params more-params)))))

(defn- create-get-obj-field-fn [create-obj-fn extract-obj-id-fields-fn]
  (fn [obj field-kw]
    (let [field-val (obj field-kw)]
      (if (nil? field-val)
        ((apply create-obj-fn (extract-obj-id-fields-fn obj)) field-kw)
        field-val))))

(defn- create-parse-one-or-more-fn [parse-one-fn extractor-fn]
  (fn [data-fn]
    (lazy-seq
      (let [data (data-fn) one-or-more (extractor-fn data)]
        (do
          (debug (str "parsing: " data))
          (if (map? one-or-more)
            (vector (parse-one-fn one-or-more))
            (vec (map parse-one-fn one-or-more))))))))

(defn- create-parse-string-or-list-fn [obj-from-name-fn extractor-fn]
  (fn [data]
    (let [string-or-list (extractor-fn data)]
      (if (string? string-or-list)
        (vector (obj-from-name-fn string-or-list))
        (vec (map obj-from-name-fn string-or-list))))))

;;;;;;;;;; forward declaration ;;;;;;;;;;

(declare bio-struct artist-struct tag-struct album-struct user-struct
  track-struct event-struct venue-struct location-struct)

(declare artist-from-name tag-from-name)

;;;;;;;;;; Bio/Wiki ;;;;;;;;;;

(defstruct bio-struct :published :summary :content)

(defn- parse-bio [data]
  (do
    (debug (str "parse-bio: " data))
    (struct
      bio-struct
      (-> data :published parse-date)
      (data :summary)
      (data :content))))

;;;;;;;;;; Location ;;;;;;;;;;

(defstruct location-struct
  :latitude :longitude :street :postalcode :city :country)

(defn- parse-location [data]
  (do
    (debug (str "parse-location: " data))
    (struct location-struct
      (-> data :geo:point :geo:lat safe-parse-double)
      (-> data :geo:point :geo:long safe-parse-double)
      (data :street)
      (data :postalcode)
      (data :city)
      (data :country))))

;;;;;;;;;; Venue ;;;;;;;;;;

(defstruct venue-struct
  :id :name :location :url :website :phonenumber)

(defn- parse-venue [data]
  (do
    (debug (str "parse-venue: " data))
    (struct venue-struct
      (data :id)
      (data :name)
      (-> data :location parse-location)
      (data :url)
      (data :website)
      (data :phonenumber))))

;;;;;;;;;; Event ;;;;;;;;;;

(defstruct event-struct
  :id :title :artists :headliner :venue :start :description
  :attendence :reviews :tag :url :website :cancelled :tags)

(def #^{:private true} parse-event-artists
  (create-parse-string-or-list-fn
    #(artist-from-name %) #(-> % :artists :artist)))

(def #^{:private true} parse-event-tags
  (create-parse-string-or-list-fn
    #(tag-from-name %) #(-> % :tags :tag)))

(defn- parse-event [data]
  (do
    (debug (str "parse-event: " data))
    (struct event-struct
      (data :id)
      (data :title)
      (parse-event-artists data)
      (artist-from-name (-> data :artists :headliner))
      (-> data :venue parse-venue)
      (-> data :startDate parse-date)
      (data :description)
      (-> data :attendence safe-parse-int)
      (-> data :reviews safe-parse-int)
      (data :tag)
      (data :url)
      (data :website)
      (= 1 (-> data :cancelled safe-parse-int))
      (parse-event-tags data))))

;;;;;;;;;; Artist ;;;;;;;;;;

(defstruct artist-struct
  :name :url :mbid :streamable :listeners :playcount :bio)

(defn- parse-artist [data]
  (do
    (debug (str "parse-artist: " data))
    (struct
      artist-struct
      (data :name)
      (data :artist :url)
      (data :mbid)
      (= 1 (-> data :streamable safe-parse-int))
      (-> data :stats :listeners safe-parse-int)
      (-> data :stats :playcount safe-parse-int)
      (-> data :bio parse-bio))))

(defn- artist-from-name [artst-name]
  (struct-map artist-struct :name artst-name))

;;;;;;;;;; artist.getinfo ;;;;;;;;;;

(defn- parse-artist-getinfo [data-fn]
  (let [data (data-fn)]
    (do
      (debug (str "parse-artist-getinfo: " data))
      (-> data :artist parse-artist))))

(def #^{:private true} get-artist
  (create-get-obj-fn {:method "artist.getinfo"} parse-artist-getinfo))

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

(defn- parse-artist-similar-1 [data]
  (struct-map artist-struct
    :name (data :name)
    :url (data :url)
    :mbid (data :mbid)
    :streamable (= 1 (-> data :streamable safe-parse-int))
    :match (-> data :match safe-parse-double)))

(def #^{:private true} parse-artist-similar
  (create-parse-one-or-more-fn
    parse-artist-similar-1
    #(-> % :similarartists :artist)))

(def #^{:private true} get-artist-similar
  (create-get-obj-fn {:method "artist.getsimilar"} parse-artist-similar))

(defn- artist-or-name [artst-or-name & _]
    (if (struct? artst-or-name) :artist :name))

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


(def #^{:private true} parse-artist-toptags
  (create-parse-one-or-more-fn
    #(struct tag-struct (% :name) (% :url))
    #(-> % :toptags :tag)))

(def #^{:private true} get-artist-toptags
  (create-get-obj-fn {:method "artist.gettoptags"} parse-artist-toptags))

(defmulti artist-toptags artist-or-name)

(defmethod artist-toptags :artist [artst]
  (-> artst :name artist-toptags))

(defmethod artist-toptags :name [artist-name]
  (get-artist-toptags {:artist artist-name}))

;;;;;;;;;; artist.gettopalbums ;;;;;;;;;;

(defn- parse-artist-topalbums-1 [data]
  (struct-map album-struct
    :name (data :name)
    :url (data :url)
    :mbid (data :mbid)
    :artist (struct-map artist-struct
              :name (-> data :artist :name)
              :url (-> data :artist :url)
              :mbid (-> data :artist :mbid))
    :playcount (-> data :playcount safe-parse-int)
    :rank (-> data attr-kw :rank safe-parse-int)))

(def #^{:private true} parse-artist-topalbums
  (create-parse-one-or-more-fn
    parse-artist-topalbums-1
    #(-> % :topalbums :album)))

(def #^{:private true} get-artist-topalbums
  (create-get-obj-fn {:method "artist.gettopalbums"} parse-artist-topalbums))

(defmulti artist-topalbums artist-or-name)

(defmethod artist-topalbums :artist [artst]
  (-> artst :name artist-topalbums))

(defmethod artist-topalbums :name [artist-name]
  (get-artist-topalbums {:artist artist-name}))

;;;;;;;;;; artist.gettopfans ;;;;;;;;;;

(defn- parse-artist-topfans-1 [data]
  (struct-map user-struct
    :name (data :name)
    :url (data :url)
    :realname (data :realname)
    :weight (-> data :weight safe-parse-int)))

(def #^{:private true} parse-artist-topfans
  (create-parse-one-or-more-fn
    parse-artist-topfans-1
    #(-> % :topfans :user)))

(def #^{:private true} get-artist-topfans
  (create-get-obj-fn {:method "artist.gettopfans"} parse-artist-topfans))

(defmulti artist-topfans artist-or-name)

(defmethod artist-topfans :artist [artst]
  (-> artst :name artist-topfans))

(defmethod artist-topfans :name [artist-name]
  (get-artist-topfans {:artist artist-name}))

;;;;;;;;;; artist.gettoptracks ;;;;;;;;;;

(defn- parse-artist-toptracks-1 [data]
  (struct-map track-struct
    :name (data :name)
    :url (data :url)
    :mbid (data :mbid)
    :artist (struct-map artist-struct
              :name (-> data :artist :name)
              :url (-> data :artist :url)
              :mbid (-> data :artist :mbid))
    :playcount (-> data :playcount safe-parse-int)
    :listeners (-> data :listeners safe-parse-int)
    :streamable (= 1 (-> data :streamable :#text safe-parse-int))
    :streamable-full (= 1 (-> data :streamable :fulltrack safe-parse-int))))

(def #^{:private true} parse-artist-toptracks
  (create-parse-one-or-more-fn
    parse-artist-toptracks-1
    #(-> % :toptracks :track)))

(def #^{:private true} get-artist-toptracks
  (create-get-obj-fn {:method "artist.gettoptracks"} parse-artist-toptracks))

(defmulti artist-toptracks artist-or-name)

(defmethod artist-toptracks :artist [artst]
  (-> artst :name artist-toptracks))

(defmethod artist-toptracks :name [artist-name]
  (get-artist-toptracks {:artist artist-name}))

;;;;;;;;;; artist.getevents ;;;;;;;;;;

(def #^{:private true} parse-artist-events
  (create-parse-one-or-more-fn
    parse-event
    #(-> % :events :event)))

(def #^{:private true} get-artist-events
  (create-get-obj-fn {:method "artist.getevents"} parse-artist-events))

(defmulti artist-events artist-or-name)

(defmethod artist-events :artist [artst]
  (-> artst :name artist-events))

(defmethod artist-events :name [artist-name]
  (get-artist-events {:artist artist-name}))

;;;;;;;;;; artist.getpastevents ;;;;;;;;;;

(declare get-artist-pastevents)

(defn- parse-artist-pastevents [data-fn]
  (lazy-seq
    (let [data (data-fn)
          pages (-> data :events attr-kw :totalPages safe-parse-int)
          page (-> data :events attr-kw :page safe-parse-int)
          artist-name (-> data :events attr-kw :artist)]
      (if (= page pages)
        (parse-artist-events data-fn)
        (lazy-cat
          (parse-artist-events data-fn)
          (get-artist-pastevents {:artist artist-name :page (inc page)}))))))

(defn- get-artist-pastevents
  ([params]
    ((create-get-obj-fn
        {:method "artist.getpastevents"}
        parse-artist-pastevents)
      params))
  ([params page]
    ((create-get-obj-fn
        {:method "artist.getpastevents" :page page}
        parse-artist-pastevents)
      params)))

(defmulti artist-pastevents artist-or-name)

(defmethod artist-pastevents :artist [artst]
  (-> artst :name artist-pastevents))

(defmethod artist-pastevents :name [artist-name]
  (get-artist-pastevents {:artist artist-name}))

;;;;;;;;;; Tag ;;;;;;;;;;

(defstruct tag-struct :name :url)

(defn- tag-from-name [tag-name]
  (struct tag-struct tag-name (lastfm-url (str "/tag/" tag-name))))

;;;;;;;;;; Album ;;;;;;;;;;

(defstruct album-struct :name :url :mbid :artist :playcount)

;;;;;;;;;; Track ;;;;;;;;;;

(defstruct track-struct
  :name :url :mbid :artist :playcount :listeners :streamable)

;;;;;;;;;; User ;;;;;;;;;;

(defstruct user-struct :name :url :realname)

(comment

(def *lastfm-api-key* "23caa86333d2cb2055fa82129802780a")
(def u2 (artist "u2"))
(println (artist-info u2 :url))

)