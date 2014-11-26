(ns csv2csv.tokenize
  (:require [clojure.data.csv :as csv])
  (:require [csv2csv.util :as util]))

;;
;; parse Lines into Rows and Cells: [Line] to [Row]
;;
(defn- safe-nth [v index]
  (if (> (count v) index)
    (nth v index)
    nil))

(defn create-cell [name value]
  (csv2csv.core.Cell. name value))

(defn- create-cells [splits tokens]
  (map (fn [^csv2csv.core.TokenSpec token]
         (if-let [index (:index token)]
           (if-let [split (safe-nth splits index)]
             (csv2csv.core.Cell. (:name token)
                                 split)
             (csv2csv.core.Cell. (:name token)
                    (:value token)))
           (csv2csv.core.Cell. (:name token)
                  (:value token))
           )
         )
       tokens))

(defn- get-char [sep]
  (if (char? sep)
    sep
    (first sep)))
;;
;; simple split the content of the Line into substring,
;; the splitting separator is defined in the configuration
;;
(defn split-line
  [^csv2csv.core.Config config line]
  (let [separator (get-char (:input-separator config))
        quote (get-char (:input-quote config))]
    (if (nil? quote)
      (first (csv/read-csv (:str line) :separator separator))
      (first (csv/read-csv (:str line) :separator separator :quote quote)))))

;;
;; split the Line into Cells (do not use the tokenspec)
;;
(defn lines-to-cells [^csv2csv.core.Spec spec lines]
  (map (fn [^csv2csv.core.Line line]
         (let [splits (split-line (:config spec) line)] 
           (csv2csv.core.Row. (:index line)
                              (map (fn [index split]
                                     (create-cell (str index) split))
                                   (range)
                                   splits))))
       lines))
;;
;; find header lines
;;
(defn- line-contains-all-words? [words line]
  "returns true if the ^Line line contains all the words"
  (every? #(util/substring? % (:str line)) words))

(defn- line-contains-any-wordgroup? [wordgroups line]
  "return true if the ^Line line contains all the words of any of the wordss"
  (some #(line-contains-all-words? % line) wordgroups))

(defn- get-lines-containing-any-wordgroup [wordgroups lines]
  (filter #(line-contains-any-wordgroup? wordgroups %) lines))
;;
;; merge different rows into a single row (concatenate the values of the cells)
;;
(defn- merge-cell [f cell1 cell2]
  (csv2csv.core.Cell. (:name cell1)
                      (f (:value cell1) (:value cell2))))

(defn- merge-row [f row1 row2]
  (csv2csv.core.Row. (:index row1)
                     (map #(merge-cell f %1 %2) (:cells row1) (:cells row2))))

(defn- merge-rows [f rows]
  "merge the rows by merging the values in each column. The merge function is given as parameter"
  (reduce (fn [acc row]
            (merge-row f acc row)) (first rows) (rest rows)))

(defn extract-column-headers [spec wordgroups lines]
  "extract the lines that match any of the wordgroups, split each line in row & cells and merge the columns of the rows"
  (->> (get-lines-containing-any-wordgroup wordgroups lines)
       (lines-to-cells spec)
       (merge-rows str) ;;merge by simple stringing the values
      )
  )

;;
;; split the Line into Cells using the token specifications
;;
(defn lines-to-rows 
  ([^csv2csv.core.Spec spec lines]
     (lines-to-rows (:config spec) (:tokens spec) lines))
  ([^csv2csv.core.Config config tokens lines]
     (map (fn [^csv2csv.core.Line line]
            (let [splits (split-line config line)] 
              (csv2csv.core.Row. (:index line)
                                 (create-cells splits tokens))))
          lines)))

;; lines-to-
