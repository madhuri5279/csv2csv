(ns csv2csv.transpose
  (require [csv2csv.core :as core]))
;;
;; input cell {:name "BRE" :value 1000}
;;
;; transpose {:header "airport"
;;            :value "domtot"
;;            :columns ["HAM" "BRE" ... ] }
;;
;; output cells {:name "airport" :balue "BRE"} {:name "domtot" :value 1000}
;;
(defn- list-contains-element? [l e]
  (some #(= e %) l))

(defn- transpose-or-not-transpose [transpose cells]
  (let [t (filter #(list-contains-element? (:columns transpose) (:name %)) cells )
        f (filter #(not (list-contains-element? (:columns transpose) (:name %))) cells)
        ]
    [t f]
   ))

(defn- transpose-cell [transpose cell]
  [(csv2csv.core.Cell. (:header-name transpose) (:name cell))
   (csv2csv.core.Cell. (:value-name transpose) (:value cell))]
    )

(defn transpose-row [transpose row]
  (let [cells (:cells row)
        d (transpose-or-not-transpose transpose cells)
        cells-to-transpose (first d)
        cells-not-to-transpose (second d)

        transposed-cells (map #(transpose-cell transpose %) cells-to-transpose)
        ]
    (map (fn [transposed-cell]
           (csv2csv.core.Row. (:index row)
                             (concat cells-not-to-transpose transposed-cell)))
         transposed-cells)))


(defn transpose-rows [^csv2csv.core.Spec spec rows]
  (if (or (nil? (:transpose spec))
          (nil? (get-in spec [:transpose :columns])))
    rows
    (flatten
     (map #(transpose-row (:transpose spec) %) rows))))




