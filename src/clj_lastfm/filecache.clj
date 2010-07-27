(ns clj-lastfm.filecache
  (:import (java.io File))
  (:use [clojure.contrib.duck-streams :only (reader copy)]
        [clojure.contrib.logging]))

(def default-cache-dir (File. (System/getProperty "java.io.tmpdir")))
(def default-expiry-time (* 24 60)) ;in minutes

(defn- minutes-to-millis [mins] (* mins 1000 60))
(defn- recently-modified? [#^File file expiry-time]
  (> (.lastModified file)
    (- (System/currentTimeMillis) (minutes-to-millis expiry-time))))

(defn create-file-cache
  "Creates a file cache"
  ([] (create-file-cache default-cache-dir default-expiry-time))
  ([cache-dir] (create-file-cache cache-dir default-expiry-time))
  ([cache-dir expiry-time]
    {:cache-dir cache-dir :expiry-time expiry-time}))

(defn get-file-content
  "Gets the content of the URL provided from cache if present, else fetches and
  caches it and returns the content"
  [{#^File cache-dir :cache-dir expiry-time :expiry-time} url]
  (let [url-hash (hash url)
        cache-file (File. (str (.getCanonicalPath cache-dir)
                               File/separator "clj-lastfm-" url-hash))]
    (do
      (info (str "getting URL: " url))
      (when-not (and (.exists cache-file) (recently-modified? cache-file expiry-time))
        (info "cache missed")
        (copy (reader url) cache-file))
      (slurp (.getCanonicalPath cache-file)))))

(comment

(def cache (create-file-cache))
(def url "http://ws.audioscrobbler.com/2.0/?method=artist.gettoptracks&artist=bon%20jovi&api_key=23caa86333d2cb2055fa82129802780a&format=json")
(println (get-file-content cache url))

)