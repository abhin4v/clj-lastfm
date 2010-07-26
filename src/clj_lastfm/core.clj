(ns clj-lastfm.core
  (:import (java.net URI)
           (java.text SimpleDateFormat)
           (java.util TimeZone))
  (:use [clj-lastfm.filecache]
        [clojure.contrib.def :only (defstruct- defvar-)]
        [clojure.contrib.json.read :only (read-json)]
        [clojure.contrib.logging]
        [clojure.contrib.math :only (ceil)]
        [clojure.walk :only (keywordize-keys)]))

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

(defn- blank? [s] (every? #(Character/isWhitespace #^Character %) s))

(defn- struct? [obj] (instance? clojure.lang.PersistentStructMap obj))

(def #^{:private true :tag SimpleDateFormat} sdftz
  (doto (SimpleDateFormat. "EEE, dd MMMM yyyy HH:mm:ss +0000")
    (.setTimeZone (TimeZone/getTimeZone "GMT"))))

(def #^{:private true :tag SimpleDateFormat} sdf
  (doto (SimpleDateFormat. "EEE, dd MMMM yyyy HH:mm:ss")
    (.setTimeZone (TimeZone/getTimeZone "GMT"))))

(defn- parse-date [date-str]
  (if (some #(% date-str) [nil? blank?])
    nil
    (try
      (.parse sdftz date-str)
      (catch java.text.ParseException e
        (.parse sdf date-str)))))

(defn- remove-nil-values [m]
  (apply hash-map (apply concat (filter #(-> % fnext nil? not) m))))

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

(defvar- default-cache (create-file-cache))

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

(defn- create-paged-get-obj-fn [fixed-params parse-fn]
  (fn
    ([more-params]
      ((create-get-obj-fn fixed-params parse-fn)
        more-params))
    ([more-params page]
      ((create-get-obj-fn fixed-params parse-fn)
        (assoc more-params :page page)))))

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

;;;;;;;;;; forward declaration ;;;;;;;;;;

(declare bio-struct artist-struct tag-struct album-struct user-struct
  track-struct event-struct venue-struct location-struct shout-struct)

(declare artist-from-name tag-from-name user-from-name)

;;;;;;;;;; Bio/Wiki ;;;;;;;;;;

(defstruct- bio-struct :published :summary :content)

(defn- parse-bio [data]
  (do
    (debug (str "parse-bio: " data))
    (struct
      bio-struct
      (-> data :published parse-date)
      (data :summary)
      (data :content))))

;;;;;;;;;; Location ;;;;;;;;;;

(defstruct- location-struct
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

(defstruct- venue-struct
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

(defstruct- event-struct
  :id :title :artists :headliner :venue :start :description
  :attendence :reviews :tag :url :website :cancelled :tags)

(defvar- parse-event-artists
  (create-parse-string-or-list-fn
    #(artist-from-name %) #(-> % :artists :artist)))

(defvar- parse-event-tags
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
      (-> data :cancelled str-1?)
      (parse-event-tags data))))

;;;;;;;;;; Shout ;;;;;;;;;;

(defstruct- shout-struct :body :author :date)

(defn- parse-shout [data]
  (do
    (debug (str "parse-shout: " data))
    (struct shout-struct
      (data :body)
      (-> data :author user-from-name)
      (-> data :date parse-date))))

;;;;;;;;;; Artist ;;;;;;;;;;

(defstruct- artist-struct
  :name :url :mbid :streamable :listeners :playcount :bio)

(defn- parse-artist [data]
  (do
    (debug (str "parse-artist: " data))
    (struct
      artist-struct
      (data :name)
      (data :url)
      (data :mbid)
      (-> data :streamable str-1?)
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

(defvar- get-artist
  (create-get-obj-fn {:method "artist.getinfo"} parse-artist-getinfo))

(defmulti artist
  (fn [artist-or-mbid & _]
    (if (mbid? artist-or-mbid) :mbid :artist)))

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
    :streamable (-> data :streamable str-1?)
    :match (-> data :match safe-parse-double)))

(defvar- parse-artist-similar
  (create-parse-one-or-more-fn
    parse-artist-similar-1 #(-> % :similarartists :artist)))

(defvar- get-artist-similar
  (create-get-obj-fn
    {:method "artist.getsimilar"} parse-artist-similar))

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

(defvar- parse-artist-toptags
  (create-parse-one-or-more-fn
    #(struct tag-struct (% :name) (% :url))
    #(-> % :toptags :tag)))

(defvar- get-artist-toptags
  (create-get-obj-fn
    {:method "artist.gettoptags"} parse-artist-toptags))

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

(defvar- parse-artist-topalbums
  (create-parse-one-or-more-fn
    parse-artist-topalbums-1 #(-> % :topalbums :album)))

(defvar- get-artist-topalbums
  (create-get-obj-fn
    {:method "artist.gettopalbums"} parse-artist-topalbums))

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

(defvar- parse-artist-topfans
  (create-parse-one-or-more-fn
    parse-artist-topfans-1 #(-> % :topfans :user)))

(defvar- get-artist-topfans
  (create-get-obj-fn
    {:method "artist.gettopfans"} parse-artist-topfans))

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
    :streamable (-> data :streamable text-kw str-1?)
    :streamable-full (-> data :streamable :fulltrack str-1?)))

(defvar- parse-artist-toptracks
  (create-parse-one-or-more-fn
    parse-artist-toptracks-1 #(-> % :toptracks :track)))

(defvar- get-artist-toptracks
  (create-get-obj-fn
    {:method "artist.gettoptracks"} parse-artist-toptracks))

(defmulti artist-toptracks artist-or-name)

(defmethod artist-toptracks :artist [artst]
  (-> artst :name artist-toptracks))

(defmethod artist-toptracks :name [artist-name]
  (get-artist-toptracks {:artist artist-name}))

;;;;;;;;;; artist.getevents ;;;;;;;;;;

(defvar- parse-artist-events
  (create-parse-one-or-more-fn parse-event #(-> % :events :event)))

(defvar- get-artist-events
  (create-get-obj-fn
    {:method "artist.getevents"} parse-artist-events))

(defmulti artist-events artist-or-name)

(defmethod artist-events :artist [artst]
  (-> artst :name artist-events))

(defmethod artist-events :name [artist-name]
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

(defmulti artist-pastevents artist-or-name)

(defmethod artist-pastevents :artist
  ([artst] (-> artst :name artist-pastevents))
  ([artst limit] (artist-pastevents (artst :name) limit)))

(defmethod artist-pastevents :name
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

(defmulti artist-shouts artist-or-name)

(defmethod artist-shouts :artist
  ([artst] (-> artst :name artist-shouts))
  ([artst limit] (artist-shouts (artst :name) limit)))

(defmethod artist-shouts :name
  ([artist-name]
    (get-artist-shouts {:artist artist-name}))
  ([artist-name limit]
    (get-artist-shouts {:artist artist-name :limit limit})))

;;;;;;;;;; artist.search ;;;;;;;;;;

(declare get-artist-search)

(defvar- parse-artist-search-unpaged
  (create-parse-one-or-more-fn
    #(struct-map artist-struct
        :name (% :name)
        :url (% :url)
        :mbid (% :mbid)
        :streamable (-> % :streamable str-1?))
    #(-> % :results :artistmatches :artist)))

(defvar- parse-artist-search
  (create-paged-search-parse-fn
    parse-artist-search-unpaged :artist #(get-artist-search %)))

(defvar- get-artist-search
  (create-paged-get-obj-fn
    {:method "artist.search"} parse-artist-search))

(defn artist-search [artist-name]
    (get-artist-search {:artist artist-name}))

;;;;;;;;;; Album ;;;;;;;;;;

(defstruct- album-struct :name :id :url :mbid :artist :playcount)

;;;;;;;;;; album.search ;;;;;;;;;;

(declare get-album-search)

(defvar- parse-album-search-unpaged
  (create-parse-one-or-more-fn
    #(struct-map album-struct
        :name (% :name)
        :url (% :url)
        :id (-> % :id safe-parse-int)
        :artist (-> % :artist artist-from-name)
        :streamable (-> % :streamable str-1?))
    #(-> % :results :albummatches :album)))

(defvar- parse-album-search
  (create-paged-search-parse-fn
    parse-album-search-unpaged :album #(get-album-search %)))

(defvar- get-album-search
  (create-paged-get-obj-fn
    {:method "album.search"} parse-album-search))

(defn album-search [album-name]
    (get-album-search {:album album-name}))

;;;;;;;;;; Tag ;;;;;;;;;;

(defstruct- tag-struct :name :url)

(defn- tag-from-name [tag-name]
  (struct tag-struct tag-name (lastfm-url (str "/tag/" tag-name))))

;;;;;;;;;; Track ;;;;;;;;;;

(defstruct- track-struct
  :name :url :mbid :artist :playcount :listeners :streamable)

;;;;;;;;;; User ;;;;;;;;;;

(defstruct- user-struct :name :url :realname)

(defn- user-from-name [user-name]
  (struct-map user-struct :name user-name))

(comment

(def *lastfm-api-key* "23caa86333d2cb2055fa82129802780a")
(def u2 (artist "u2"))
(println (artist-info u2 :url))

)
