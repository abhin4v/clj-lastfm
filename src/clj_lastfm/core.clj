(ns clj-lastfm.core
  (:import (java.net URI)
           (java.text SimpleDateFormat)
           (java.util TimeZone))
  (:use [clj-lastfm.filecache]
        [clojure.contrib.core :only (-?> -?>>)]
        [clojure.contrib.def :only (defvar-)]
        [clojure.contrib.json :only (read-json)]
        [clojure.contrib.logging]
        [clojure.contrib.math :only (ceil)]
        [clojure.string :only (blank? trim)]))

;;;;;;;;;; Basic ;;;;;;;;;;

(defvar- api-root-url ["http" "ws.audioscrobbler.com" "/2.0/"])

(defn- api-key []
  (let [lastfm-api-key (resolve '*lastfm-api-key*)]
    (if (nil? lastfm-api-key)
      (throw (IllegalStateException. "lastfm API key is not set"))
      lastfm-api-key)))

(defvar- guid-pattern
  (let [an "[0-9a-fA-F]"]
    (re-pattern
      (str "^" an "{8}-" an "{4}-" an "{4}-" an "{4}-" an "{12}$"))))

(defn- mbid? [s] (if (re-matches guid-pattern s) true false))

(defvar- attr-kw (keyword "@attr"))

(defvar- text-kw (keyword "#text"))

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

(defn- str-1? [n] (-> n safe-parse-int (= 1)))

(defn- struct? [obj] (instance? clojure.lang.PersistentStructMap obj))

(def #^{:private true :tag SimpleDateFormat} full-date-format
  (doto (SimpleDateFormat. "EEE, dd MMMM yyyy HH:mm:ss +0000")
    (.setTimeZone (TimeZone/getTimeZone "GMT"))))

(def #^{:private true :tag SimpleDateFormat} date-format-wo-tz
  (doto (SimpleDateFormat. "EEE, dd MMMM yyyy HH:mm:ss")
    (.setTimeZone (TimeZone/getTimeZone "GMT"))))

(def #^{:private true :tag SimpleDateFormat} short-date-format-wo-tz
  (doto (SimpleDateFormat. "dd MMM yyyy, HH:mm")
    (.setTimeZone (TimeZone/getTimeZone "GMT"))))

(defn- parse-date [date-str]
  (if (some #(% date-str) [nil? blank?])
    nil
    (let [clean-date-str (trim date-str)]
      (try
        (.parse full-date-format clean-date-str)
        (catch java.text.ParseException e
          (try
            (.parse date-format-wo-tz clean-date-str)
            (catch java.text.ParseException e
              (.parse short-date-format-wo-tz clean-date-str))))))))

(defn- remove-nil-values [m]
  (apply hash-map (apply concat (filter #(-> % fnext nil? not) m))))

(defn- lastfm-url [path]
  (.toString (URI. "http" "www.last.fm" path nil nil)))

(defn- arg-types [& args] (->> args (map class) vec))

(defn- first-arg-type [& args] (->> args first class))

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

(defvar- default-cache (create-file-cache))

(defn- get-url
  ([url] (get-url url default-cache))
  ([url cache]
    (do
      (info (str "URL: " url))
      (get-file-content cache url))))

(defn- get-data [params]
  (let [url (create-url params)
        data (-> url get-url read-json)]
    (if (-> data :error nil?)
      data
      (throw (IllegalArgumentException. (-> data :message str))))))

(defn- create-get-obj-fn [fixed-params parse-fn]
  (fn [more-params]
    (parse-fn #(get-data (merge fixed-params more-params)))))

(defn- create-paged-get-obj-fn [fixed-params parse-fn]
  (fn
    ([more-params]
      ((create-get-obj-fn fixed-params parse-fn)
        more-params))
    ([more-params page]
      ((create-get-obj-fn fixed-params parse-fn)
        (assoc more-params :page page)))))

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

(defn- create-paged-parse-fn
  [pages-fn page-fn perpage-fn param-fn parse-unpaged-fn get-fn]
  (fn [data-fn]
    (lazy-seq
      (let [data (data-fn)
            pages (-> data pages-fn safe-parse-int)
            page (-> data page-fn safe-parse-int)
            limit (-> data perpage-fn safe-parse-int)
            params (param-fn data)]
        (if (= page pages)
          (parse-unpaged-fn data-fn)
          (lazy-cat
            (parse-unpaged-fn data-fn)
            (get-fn (merge params {:page (inc page) :limit limit}))))))))

(defn- create-paged-result-parse-fn
  [page-info-fn param-fn parse-unpaged-fn get-fn]
  (create-paged-parse-fn
    #(-> % page-info-fn :totalPages)
    #(-> % page-info-fn :page)
    #(-> % page-info-fn :perPage)
    param-fn parse-unpaged-fn get-fn))

(defn- create-paged-search-parse-fn
  [parse-unpaged-fn search-key get-fn]
  (create-paged-parse-fn
    #(-> (/ (-> % :results :opensearch:totalResults safe-parse-int)
            (-> % :results :opensearch:itemsPerPage safe-parse-double))
         ceil int str)
    #(-> % :results :opensearch:Query :startPage)
    #(-> % :results :opensearch:itemsPerPage)
    #(hash-map search-key (-> % :results :opensearch:Query :searchTerms))
    parse-unpaged-fn get-fn))

(defn- record-from-map [record-class no-of-fields field-map]
  (merge
    (eval `(new ~record-class ~@(repeat no-of-fields nil)))
    field-map))

;;;;;;;;;; record defs ;;;;;;;;;;

(defrecord Bio [published summary content])

(defrecord Location [latitude longitude street postalcode city country])

(defrecord Venue [id name location url website phonenumber])

(defrecord Event [id title artists headliner venue start description
  attendence reviews tag url website cancelled tags])

(defrecord Shout [body author date])

(defrecord Artist [name url mbid streamable listeners playcount bio])

(defrecord Album
  [name id url mbid artist listeners playcount release-date wiki])

(defrecord Track
  [name url mbid artist playcount listeners streamable])

(defrecord Tag [name url])

(defrecord User [name url realname])

;;;;;;;;;; forward declaration ;;;;;;;;;;

(declare artist-from-name tag-from-name user-from-name
  album-from-map user-from-map track-from-map tag-from-map)

;;;;;;;;;; Bio/Wiki ;;;;;;;;;;

(defn- parse-bio [data]
  (do
    (debug (str "parse-bio: " data))
    (Bio.
      (-> data :published parse-date)
      (data :summary)
      (data :content))))

;;;;;;;;;; Location ;;;;;;;;;;

(defn- parse-location [data]
  (do
    (debug (str "parse-location: " data))
    (Location.
      (-> data :geo:point :geo:lat safe-parse-double)
      (-> data :geo:point :geo:long safe-parse-double)
      (data :street)
      (data :postalcode)
      (data :city)
      (data :country))))

;;;;;;;;;; Venue ;;;;;;;;;;

(defn- parse-venue [data]
  (do
    (debug (str "parse-venue: " data))
    (Venue.
      (data :id)
      (data :name)
      (-> data :location parse-location)
      (data :url)
      (data :website)
      (data :phonenumber))))

;;;;;;;;;; Event ;;;;;;;;;;

(defvar- parse-event-artists
  (create-parse-string-or-list-fn
    #(artist-from-name %) #(-> % :artists :artist)))

(defvar- parse-event-tags
  (create-parse-string-or-list-fn
    #(tag-from-name %) #(-> % :tags :tag)))

(defn- parse-event [data]
  (do
    (debug (str "parse-event: " data))
    (Event.
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
      (-> data :cancelled str-1?)
      (parse-event-tags data))))

;;;;;;;;;; Shout ;;;;;;;;;;

(defn- parse-shout [data]
  (do
    (debug (str "parse-shout: " data))
    (Shout.
      (data :body)
      (-> data :author user-from-name)
      (-> data :date parse-date))))

;;;;;;;;;; Artist ;;;;;;;;;;

(defn- parse-artist [data]
  (do
    (debug (str "parse-artist: " data))
    (Artist.
      (data :name)
      (data :url)
      (data :mbid)
      (-> data :streamable str-1?)
      (-> data :stats :listeners safe-parse-int)
      (-> data :stats :playcount safe-parse-int)
      (-> data :bio parse-bio))))

(defvar- artist-from-map (partial record-from-map Artist 7))

(defn- artist-from-name [artist-name] (artist-from-map {:name artist-name}))

;;;;;;;;;; artist.getinfo ;;;;;;;;;;

(defn- parse-artist-getinfo [data-fn]
  (let [data (data-fn)]
    (do
      (debug (str "parse-artist-getinfo: " data))
      (-> data :artist parse-artist))))

(defvar- get-artist
  (create-get-obj-fn {:method "artist.getinfo"} parse-artist-getinfo))

(defmulti artist-info
  (fn [arg1 & _]
    (cond
      (instance? Artist arg1) :artist
      (mbid? arg1) :mbid
      :else :artist-name)))

(defmethod artist-info :artist
  ([artist] (-> artist :name artist-info))
  ([artist username] (-> artist :name (artist-info username)))
  ([artist username lang] (-> artist :name (artist-info username lang))))

(defmethod artist-info :artist-name
  ([artist-name] (artist-info artist-name nil nil))
  ([artist-name username] (artist-info artist-name username nil))
  ([artist-name username lang]
    (get-artist {:artist artist-name :username username :lang lang})))

(defmethod artist-info :mbid
  ([mbid] (artist-info mbid nil nil))
  ([mbid username] (artist-info mbid username nil))
  ([mbid username lang]
    (get-artist {:mbid mbid :username username :lang lang})))

;;;;;;;;;; artist.getsimilar ;;;;;;;;;;

(defn- parse-artist-similar-1 [data]
  (artist-from-map
    {:name (data :name)
     :url (data :url)
     :mbid (data :mbid)
     :streamable (-> data :streamable str-1?)
     :match (-> data :match safe-parse-double)}))

(defvar- parse-artist-similar
  (create-parse-one-or-more-fn
    parse-artist-similar-1 #(-> % :similarartists :artist)))

(defvar- get-artist-similar
  (create-get-obj-fn
    {:method "artist.getsimilar"} parse-artist-similar))

(defmulti artist-similar first-arg-type)

(defmethod artist-similar Artist
  ([artist] (-> artist :name artist-similar))
  ([artist limit] (-> artist :name (artist-similar limit))))

(defmethod artist-similar String
  ([artist-name]
    (get-artist-similar {:artist artist-name}))
  ([artist-name limit]
    (get-artist-similar {:artist artist-name :limit limit})))

;;;;;;;;;; artist.gettoptags ;;;;;;;;;;

(defvar- parse-artist-toptags
  (create-parse-one-or-more-fn
    #(Tag. (% :name) (% :url)) #(-> % :toptags :tag)))

(defvar- get-artist-toptags
  (create-get-obj-fn
    {:method "artist.gettoptags"} parse-artist-toptags))

(defmulti artist-toptags first-arg-type)

(defmethod artist-toptags Artist [artist]
  (-> artist :name artist-toptags))

(defmethod artist-toptags String [artist-name]
  (get-artist-toptags {:artist artist-name}))

;;;;;;;;;; artist.gettopalbums ;;;;;;;;;;

(defn- parse-artist-topalbums-1 [data]
  (album-from-map
    {:name (data :name)
     :url (data :url)
     :mbid (data :mbid)
     :artist (artist-from-map
              {:name (-> data :artist :name)
               :url (-> data :artist :url)
               :mbid (-> data :artist :mbid)})
     :playcount (-> data :playcount safe-parse-int)
     :rank (-> data attr-kw :rank safe-parse-int)}))

(defvar- parse-artist-topalbums
  (create-parse-one-or-more-fn
    parse-artist-topalbums-1 #(-> % :topalbums :album)))

(defvar- get-artist-topalbums
  (create-get-obj-fn
    {:method "artist.gettopalbums"} parse-artist-topalbums))

(defmulti artist-topalbums first-arg-type)

(defmethod artist-topalbums Artist [artist]
  (-> artist :name artist-topalbums))

(defmethod artist-topalbums String [artist-name]
  (get-artist-topalbums {:artist artist-name}))

;;;;;;;;;; artist.gettopfans ;;;;;;;;;;

(defn- parse-artist-topfans-1 [data]
  (user-from-map
    {:name (data :name)
     :url (data :url)
     :realname (data :realname)
     :weight (-> data :weight safe-parse-int)}))

(defvar- parse-artist-topfans
  (create-parse-one-or-more-fn
    parse-artist-topfans-1 #(-> % :topfans :user)))

(defvar- get-artist-topfans
  (create-get-obj-fn
    {:method "artist.gettopfans"} parse-artist-topfans))

(defmulti artist-topfans first-arg-type)

(defmethod artist-topfans Artist [artist]
  (-> artist :name artist-topfans))

(defmethod artist-topfans String [artist-name]
  (get-artist-topfans {:artist artist-name}))

;;;;;;;;;; artist.gettoptracks ;;;;;;;;;;

(defn- parse-artist-toptracks-1 [data]
  (track-from-map
    {:name (data :name)
     :url (data :url)
     :mbid (data :mbid)
     :artist (artist-from-map
              {:name (-> data :artist :name)
               :url (-> data :artist :url)
               :mbid (-> data :artist :mbid)})
     :playcount (-> data :playcount safe-parse-int)
     :listeners (-> data :listeners safe-parse-int)
     :streamable (-> data :streamable text-kw str-1?)
     :streamable-full (-> data :streamable :fulltrack str-1?)}))

(defvar- parse-artist-toptracks
  (create-parse-one-or-more-fn
    parse-artist-toptracks-1 #(-> % :toptracks :track)))

(defvar- get-artist-toptracks
  (create-get-obj-fn
    {:method "artist.gettoptracks"} parse-artist-toptracks))

(defmulti artist-toptracks first-arg-type)

(defmethod artist-toptracks Artist [artist]
  (-> artist :name artist-toptracks))

(defmethod artist-toptracks String [artist-name]
  (get-artist-toptracks {:artist artist-name}))

;;;;;;;;;; artist.getevents ;;;;;;;;;;

(defvar- parse-artist-events
  (create-parse-one-or-more-fn parse-event #(-> % :events :event)))

(defvar- get-artist-events
  (create-get-obj-fn
    {:method "artist.getevents"} parse-artist-events))

(defmulti artist-events first-arg-type)

(defmethod artist-events Artist [artist]
  (-> artist :name artist-events))

(defmethod artist-events String [artist-name]
  (get-artist-events {:artist artist-name}))

;;;;;;;;;; artist.getpastevents ;;;;;;;;;;

(declare get-artist-pastevents)

(defvar- parse-artist-pastevents
  (create-paged-result-parse-fn
    #(-> % :events attr-kw)
    #(hash-map :artist (-> % :events attr-kw :artist))
    parse-artist-events
    #(get-artist-pastevents %)))

(defvar- get-artist-pastevents
  (create-paged-get-obj-fn
    {:method "artist.getpastevents"} parse-artist-pastevents))

(defmulti artist-pastevents first-arg-type)

(defmethod artist-pastevents Artist
  ([artist] (-> artist :name artist-pastevents))
  ([artist limit] (-> artist :name (artist-pastevents limit))))

(defmethod artist-pastevents String
  ([artist-name]
    (get-artist-pastevents {:artist artist-name}))
  ([artist-name limit]
    (get-artist-pastevents {:artist artist-name :limit limit})))

;;;;;;;;;; artist.getshouts ;;;;;;;;;;

(declare get-artist-shouts)

(defvar- parse-artist-shouts-unpaged
  (create-parse-one-or-more-fn parse-shout #(-> % :shouts :shout)))

(defvar- parse-artist-shouts
  (create-paged-result-parse-fn
    #(-> % :shouts attr-kw)
    #(hash-map :artist (-> % :shouts attr-kw :artist))
    parse-artist-shouts-unpaged
    #(get-artist-shouts %)))

(defvar- get-artist-shouts
  (create-paged-get-obj-fn
    {:method "artist.getshouts"} parse-artist-shouts))

(defmulti artist-shouts first-arg-type)

(defmethod artist-shouts Artist
  ([artist] (-> artist :name artist-shouts))
  ([artist limit] (-> artist :name (artist-shouts limit))))

(defmethod artist-shouts String
  ([artist-name]
    (get-artist-shouts {:artist artist-name}))
  ([artist-name limit]
    (get-artist-shouts {:artist artist-name :limit limit})))

;;;;;;;;;; artist.search ;;;;;;;;;;

(declare get-artist-search)

(defvar- parse-artist-search-unpaged
  (create-parse-one-or-more-fn
    #(artist-from-map
        {:name (% :name)
         :url (% :url)
         :mbid (% :mbid)
         :streamable (-> % :streamable str-1?)})
    #(-> % :results :artistmatches :artist)))

(defvar- parse-artist-search
  (create-paged-search-parse-fn
    parse-artist-search-unpaged :artist #(get-artist-search %)))

(defvar- get-artist-search
  (create-paged-get-obj-fn
    {:method "artist.search"} parse-artist-search))

(defn artist-search
  ([artist-name]
    (get-artist-search {:artist artist-name}))
  ([artist-name limit]
    (get-artist-search {:artist artist-name :limit limit})))

;;;;;;;;;; Album ;;;;;;;;;;

(defvar- album-from-map (partial record-from-map Album 9))

(defn- parse-album [data]
  (merge
    (Album.
      (:name data)
      (-> data :id safe-parse-int)
      (:url data)
      (:mbid data)
      (-> data :artist artist-from-name)
      (-> data :listeners safe-parse-int)
      (-> data :playcount safe-parse-int)
      (-> data :releasedate parse-date)
      (-?> data :wiki parse-bio))
    {:user-playcount
      (-?> data :userplaycount safe-parse-int)}))

;;;;;;;;;; album.getinfo ;;;;;;;;;;

(defn- parse-album-getinfo [data-fn]
  (let [data (data-fn)]
    (do
      (debug (str "parse-album-getinfo: " data))
      (-> data :album parse-album))))

(defvar- get-album
  (create-get-obj-fn {:method "album.getinfo"} parse-album-getinfo))

(defmulti album-info
  (fn [arg1 & rest-args]
    (cond
      (instance? Album arg1) :album
      (mbid? arg1) :mbid
      (and (string? arg1)
           (-?>> rest-args first (instance? Artist))) :album-name-artist
      :else :album-name-artist-name)))

(defmethod album-info :album
  ([album]
    (album-info album nil nil))
  ([album username]
    (album-info album username nil))
  ([album username lang]
    (album-info (-> album :name) (-> album :artist) username lang)))

(defmethod album-info :mbid
  ([mbid]
    (album-info mbid nil nil))
  ([mbid username]
    (album-info mbid username nil))
  ([mbid username lang]
    (get-album {:mbid mbid :username username :lang lang})))

(defmethod album-info :album-name-artist
  ([album-name artist]
    (album-info album-name artist nil nil))
  ([album-name artist username]
    (album-info album-name artist username nil))
  ([album-name artist username lang]
    (album-info album-name (:name artist) username lang)))

(defmethod album-info :album-name-artist-name
  ([album-name artist-name]
    (album-info album-name artist-name nil nil))
  ([album-name artist-name username]
    (album-info album-name artist-name username nil))
  ([album-name artist-name username lang]
    (get-album
      {:album album-name :artist artist-name :username username :lang lang})))

;;;;;;;;;; album.gettoptags ;;;;;;;;;;

(defvar- parse-album-toptags
  (create-parse-one-or-more-fn
    #(tag-from-map
      {:name (% :name) :url (% :url) :count (% :count)})
    #(-> % :toptags :tag)))

(defvar- get-album-toptags
  (create-get-obj-fn
    {:method "album.gettoptags"} parse-album-toptags))

(defmulti album-toptags arg-types)

(defmethod album-toptags [Album] [album]
  (album-toptags (-> album :name) (-> album :artist :name)))

(defmethod album-toptags [String Artist] [album-name artist]
  (album-toptags album-name (-> artist :name)))

(defmethod album-toptags [String String] [album-name artist-name]
  (get-album-toptags {:album album-name :artist artist-name}))

;;;;;;;;;; album.search ;;;;;;;;;;

(declare get-album-search)

(defvar- parse-album-search-unpaged
  (create-parse-one-or-more-fn
    #(album-from-map
        {:name (% :name)
         :url (% :url)
         :id (-> % :id safe-parse-int)
         :artist (-> % :artist artist-from-name)
         :streamable (-> % :streamable str-1?)})
    #(-> % :results :albummatches :album)))

(defvar- parse-album-search
  (create-paged-search-parse-fn
    parse-album-search-unpaged :album #(get-album-search %)))

(defvar- get-album-search
  (create-paged-get-obj-fn
    {:method "album.search"} parse-album-search))

(defn album-search
  ([album-name]
    (get-album-search {:album album-name}))
  ([album-name limit]
    (get-album-search {:album album-name :limit limit})))

;;;;;;;;;; Tag ;;;;;;;;;;

(defvar- tag-from-map (partial record-from-map Tag 2))

(defn- tag-from-name [tag-name]
  (Tag. tag-name (lastfm-url (str "/tag/" tag-name))))

;;;;;;;;;; Track ;;;;;;;;;;

(defvar- track-from-map (partial record-from-map Track 7))

;;;;;;;;;; User ;;;;;;;;;;

(defvar- user-from-map (partial record-from-map User 3))

(defn- user-from-name [user-name] (user-from-map {:name user-name}))

(comment

(def *lastfm-api-key* "23caa86333d2cb2055fa82129802780a")
(def u2 (artist "u2"))
(println (artist-info u2 :url))

)
