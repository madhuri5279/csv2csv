(ns csv2csv.repeat-down
  (:require [csv2csv.core :as core])
  (:require [csv2csv.util :as util]))

;;
;; build-in repeat-down function
;;
(defn empty-cell? []
  (fn [^csv2csv.core.Config config ^csv2csv.core.Cell cell]
    (empty? (clojure.string/trim (:value cell)))))
;;
;; Repeat-down functionality: [Row] to [Row]
;

(defn- repeat-down-cells [^csv2csv.core.Spec spec ^csv2csv.core.Cell prev ^csv2csv.core.Cell curr]
  (if-let [token (first (util/get-tokens-with-name (:tokens spec) (:name curr)))]
    (if-let [fn-repeat (:repeat-down token)]
      (if (fn-repeat (:config spec) curr)
        (merge curr {:value (:value prev)})
        curr)
      curr)
    curr))

(defn- repeat-down-row [^csv2csv.core.Spec spec ^csv2csv.core.Row prev ^csv2csv.core.Row curr]
  (if (nil? prev)
    curr
    (csv2csv.core.Row. (:index curr)
          (map (fn [prev-cell curr-cell]
                 (repeat-down-cells spec prev-cell curr-cell))
               (:cells prev)
               (:cells curr)))))

(defn- repeat? [^csv2csv.core.Spec spec]
  (some #(not (nil? (:repeat-down %))) (:tokens spec)))

(defn repeat-down-rows
  ([^csv2csv.core.Spec spec rows]
     (if (repeat? spec)
       (repeat-down-rows spec nil rows)
       rows))
  ([^csv2csv.core.Spec spec ^csv2csv.core.Row prev rows]
     (lazy-seq
      (when-let [s (seq rows)]
        (let [row* (repeat-down-row spec prev (first rows))]
          (cons row*
                (repeat-down-rows spec row* (rest rows))))))))
