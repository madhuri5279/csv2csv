(ns csv2csv.util
  (:require [csv2csv.core :as core]))

(defn substring? [sub s]
  (.contains s sub))

;;
;; helper functions
;;
(defn compose-skip-functions [fs]
  (if (vector? fs)
    (fn [^csv2csv.core.Config config ^csv2csv.core.Line line]
      (loop [value ((first fs) config line) ;; check for special values
             fs (next fs) ]
        (if (nil? fs)
          value
          (recur (or value ((first fs) config line)) (next fs)))
        ))
    (fn [^csv2csv.core.Config config ^csv2csv.core.Line line]
      (fs config line))
    ))

(defn compose-functions [fs]
  (if (vector? fs)
    (fn [^csv2csv.core.Config config ^csv2csv.core.Cell cell ^csv2csv.core.Row row]
      (loop [value ((first fs) config cell row) ;; check for special values
             fs (next fs) ]
        (if (nil? fs)
            value
            (recur ((first fs) config (merge cell {:value value}) row) (next fs)))
        ))
    (fn [^csv2csv.core.Config config ^csv2csv.core.Cell cell ^csv2csv.core.Row row]
      (fs config cell row))
    ))

(defn is-functions? [fs]
  (or (fn? fs)
      (and (vector? fs)
           (every? fn? fs))))


(defn get-tokens-with-name [tokens name]
  (filter #(= name (:name %)) tokens))

(defn get-tokens-with-index [tokens index]
  (filter #(= index (:index %)) tokens))


