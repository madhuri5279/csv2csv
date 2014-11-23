(ns csv2csv.core-test
  (:use [clojure.test :refer :all]
        [csv2csv.core :as core]))


(defn extract-airport []
  (fn [^csv2csv.core.Config config ^csv2csv.core.Cell cell ^csv2csv.core.Row row]
    (first (clojure.string/split (:value cell) #"-"))))

(defn extract-country []
  (fn [^csv2csv.core.Config config ^csv2csv.core.Cell cell ^csv2csv.core.Row row]
    (second (clojure.string/split (:value cell) #"-"))))

(def spec
  {:config {:input-separator \,
            :output-separator \^
            :decimal-separator \space
            }

   ;; skip is not a transformation function because a lot of headers (which we want to skip) are not properly formatted in columns. The skip functions work on the full text of the row
   ;;
   ;; :skip can be a simple function or a vector of functions
   ;; each functions take ^Config and ^Line are arguments and returns a boolean
   ;; or (line-contains? "identifier|total") use string 
   ;; or (line-contains? #"identifier|total") use regex
   :skip [(line-contains? "identifier") 
          (line-contains? "total")
          (line-empty?)]
   ;;:skip (line-contains? #"identifier|total")

   :tokens [{:index 0 :name "id" :tx [(convert-to-int) (skip-if-equal 0)]}
            {:index 1 :name "airport" :tx (extract-airport)}
            {:index 1 :name "country" :tx (extract-country)}
            {:index 2 :name "total" :tx (convert-to-int)}
            {:index 3 :name "source" :repeat-down (empty-cell?) :tx [(trim) (skip-if-equal "--")]}
            {:name "date" :value "now"}
            ]

   })

(def simple-lines
  ["identifier,airport-country,total,source"
   ""
   "1,BRU-BE,1000,WEB  "
   "2,CDG-FR,2000,"
   "3,FRA-DE,3000, FILE"
   "4,MAD-ES,4000, "
   "0,------,----,--"
   "total,,10000"
   ])


(defn- get-value-cell [^csv2csv.core.Row row ^String name]
  (->> row
       :cells
       (filter #(= name (:name %)))
       (first)
       :value))

(deftest a-test
  (testing "simple specification"
    (let [spec* (create-spec spec)
          lines* (strings-to-lines (:config spec) simple-lines)
          filtered-lines (skip-lines spec* lines*)
          rows (lines-to-rows spec* filtered-lines)
          rows* (repeat-down-rows spec* rows)
          rows** (process-rows spec* rows*)
          ]
      (are [x y] (= x y)
           8 (count lines*)
           5 (count filtered-lines)
           5 (count rows*)
           4 (count rows**)
           "BRU" (get-value-cell (first rows**) "airport")
           "BE"  (get-value-cell (first rows**) "country")
           "WEB" (get-value-cell (first rows**) "source")
           1000 (get-value-cell (first rows**) "total")
           
           "MAD" (get-value-cell (last rows**) "airport")
           "ES"  (get-value-cell (last rows**) "country")
           "FILE" (get-value-cell (last rows**) "source")
           4000 (get-value-cell (last rows**) "total")
           ))))



