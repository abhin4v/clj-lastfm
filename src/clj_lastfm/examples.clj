(ns clj-lastfm.examples
  (:use [clj-lastfm.core]))

(defn- indexed [s] (map vector (iterate inc 1) s))

(defn print-topten-tracks [artist-name]
  (doseq [[idx name]
            (->> artist-name artist-toptracks (take 10) (map :name) indexed)]
    (println (format "%s. %s" idx name))))

(defn print-topten-album-search-results [album-name]
  (doseq [[idx [name artist]]
            (->> album-name album-search (take 10)
              (map #(vector (:name %) (-> % :artist :name)))
              indexed)]
    (-> "%s. \"%s\" by %s" (format idx name artist) println)))
