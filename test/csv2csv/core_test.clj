(ns csv2csv.core-test
  (:use [clojure.test :refer :all])
  (:require [csv2csv.core :as core]
            [csv2csv.input :as input]
            [csv2csv.skip :as skip]
            [csv2csv.tokenize :as tokenize]
            [csv2csv.repeat-down :as repeat]
            [csv2csv.tx :as tx]
            [csv2csv.transpose :as transpose]))


(defn extract-airport []
  (fn [^csv2csv.core.Config config ^csv2csv.core.Cell cell ^csv2csv.core.Row row]
    (first (clojure.string/split (:value cell) #"-"))))

(defn extract-country []
  (fn [^csv2csv.core.Config config ^csv2csv.core.Cell cell ^csv2csv.core.Row row]
    (second (clojure.string/split (:value cell) #"-"))))

(def simple-lines
  ["identifier,airport-country,total,source"
   ""
   "1,BRU-BE,1000,WEB  "
   "2,CDG-FR,2000,"
   "3,FRA-DE,3000, FILE"
   "4,MAD-ES,4000, "
   "0,------,----,--"
   "total,,10000"
   "Comment"
   "5,NCE-BOS,1000,"
   ""
   ])

(def simple-spec
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
   :skip [(skip/line-contains? "identifier") 
          (skip/line-contains? "total")
          (skip/line-empty?)]
   ;;:skip (line-contains? #"identifier|total")

;;   :stop (skip/line-contains? "Comment")
   :stop [(skip/line-contains? "Comment")]
   
   :tokens [{:index 0 :name "id" :tx [(tx/convert-to-int) (tx/skip-if-equal 0)]}
            {:index 1 :name "airport" :tx (extract-airport)}
            {:index 1 :name "country" :tx (extract-country)}
            {:index 2 :name "total" :tx (tx/convert-to-int)}
            {:index 3 :name "source" :repeat-down (repeat/empty-cell?) :tx [(tx/trim) (tx/skip-if-equal "--")]}
            {:name "date" :value "now"}
            {:name "copy" :tx [(tx/copy-from-cell "source") (tx/trim)]} ;; cells are process in parallel (map), so apply trim
            ]

   })

(def simple-lines-with-start
  ["9,BOS-US,10000,"
   "4,MAD-ES,4000, "
   "identifier,airport-country,total,source"
   ""
   "1,BRU-BE,1000,WEB  "
   "2,CDG-FR,2000,"
   "3,FRA-DE,3000, FILE"
   "4,MAD-ES,4000, "
   "0,------,----,--"
   "total,,10000"
   "Comment"
   "5,NCE-BOS,1000,"
   ""
   ])

(def simple-spec-with-start
  {:config {:input-separator \,
            :output-separator \^
            :decimal-separator \space
            }

   :start [(skip/line-contains? "identifier") (skip/line-contains? "source")]
   
   :skip [(skip/line-contains? "identifier") 
          (skip/line-contains? "total")
          (skip/line-empty?)]

   :stop [(skip/line-contains? "Comment")]
   
   :tokens [{:index 0 :name "id" :tx [(tx/convert-to-int) (tx/skip-if-equal 0)]}
            {:index 1 :name "airport" :tx (extract-airport)}
            {:index 1 :name "country" :tx (extract-country)}
            {:index 2 :name "total" :tx (tx/convert-to-int)}
            {:index 3 :name "source" :repeat-down (repeat/empty-cell?) :tx [(tx/trim) (tx/skip-if-equal "--")]}
            {:name "date" :value "now"}
            {:name "copy" :tx [(tx/copy-from-cell "source") (tx/trim)]} ;; cells are process in parallel (map), so apply trim
            ]

   })


(defn- get-value-cell [^csv2csv.core.Row row ^String name]
  (->> row
       :cells
       (filter #(= name (:name %)))
       (first)
       :value))

(deftest a-test
  (testing "simple specification"
    (let [spec* (core/create-spec simple-spec)
          lines* (input/strings-to-lines (:config spec*) simple-lines)
          filtered-lines (skip/skip-lines spec* lines*)
          filtered-lines* (skip/stop-lines spec* filtered-lines)
          rows (tokenize/lines-to-rows spec* filtered-lines*)
          rows* (repeat/repeat-down-rows spec* rows)
          rows** (tx/process-rows spec* rows*)
          rows*** (transpose/transpose-rows spec* rows**)
          ;; spec* (core/create-spec simple-spec)
          ;; rows*** (->> simple-lines
          ;;              (input/strings-to-lines (:config spec*))
          ;;              (skip/skip-lines spec*)
          ;;              (skip/stop-lines spec*)
          ;;              (tokenize/lines-to-rows spec*)
          ;;              (repeat/repeat-down-rows spec*)
          ;;              (tx/process-rows spec*)
          ;;              (transpose/transpose-rows spec*))
          ]
      (are [x y] (= x y)
           11 (count lines*)
           7 (count filtered-lines)
           5 (count filtered-lines*)
           5 (count rows*)
           4 (count rows**)
           4 (count rows***)
           "BRU" (get-value-cell (first rows***) "airport")
           "BE"  (get-value-cell (first rows***) "country")
           "WEB" (get-value-cell (first rows***) "source")
           1000 (get-value-cell (first rows***) "total")
           "WEB" (get-value-cell (first rows***) "copy")
           
           "MAD" (get-value-cell (last rows**) "airport")
           "ES"  (get-value-cell (last rows**) "country")
           "FILE" (get-value-cell (last rows**) "source")
           4000 (get-value-cell (last rows**) "total")
           "FILE" (get-value-cell (last rows**) "copy")
           ))))


(deftest a-test-with-start
  (testing "simple specification with start"
    (let [spec* (core/create-spec simple-spec-with-start)
          lines* (input/strings-to-lines (:config spec*) simple-lines-with-start)
          lines** (skip/start-at-lines spec* lines*)
          filtered-lines (skip/skip-lines spec* lines**)
          filtered-lines* (skip/stop-lines spec* filtered-lines)
          rows (tokenize/lines-to-rows spec* filtered-lines*)
          rows* (repeat/repeat-down-rows spec* rows)
          rows** (tx/process-rows spec* rows*)
          rows*** (transpose/transpose-rows spec* rows**)
          ]
      (are [x y] (= x y)
           13 (count lines*)
           7 (count filtered-lines)
           5 (count filtered-lines*)
           5 (count rows*)
           4 (count rows**)
           4 (count rows***)
           "BRU" (get-value-cell (first rows***) "airport")
           "BE"  (get-value-cell (first rows***) "country")
           "WEB" (get-value-cell (first rows***) "source")
           1000 (get-value-cell (first rows***) "total")
           "WEB" (get-value-cell (first rows***) "copy")
           
           "MAD" (get-value-cell (last rows**) "airport")
           "ES"  (get-value-cell (last rows**) "country")
           "FILE" (get-value-cell (last rows**) "source")
           4000 (get-value-cell (last rows**) "total")
           "FILE" (get-value-cell (last rows**) "copy")
           ))))

(def transpose-lines
  ["origin, 1, 2, 3, 4"
   "A     ,A1,A2,A3,A4"
   "B     ,B1,B2,B3,B4"
   "C     ,C1,C2,C3,C4"
   ])

(def transpose-spec
  {:config {:input-separator \,
            :output-separator \^
            :decimal-separator \space
            }

   :tokens [{:index 0 :name "origin" :tx [(tx/trim) (tx/skip-if-equal "origin")]}
            {:index 1 :name "1" :tx (tx/trim)}
            {:index 2 :name "2" :tx (tx/trim)}
            {:index 3 :name "3" :tx (tx/trim)}
            {:index 4 :name "4" :tx (tx/trim)}
           
            ]
   :transpose {:header-name "destination"
               :value-name "position"
               :columns ["1" "2" "3" "4"]}
   
   })

(deftest transpose
  (testing "transposing some columns"
        (let [spec* (core/create-spec transpose-spec)
          lines* (input/strings-to-lines (:config spec*) transpose-lines)
          filtered-lines (skip/skip-lines spec* lines*)
          rows (tokenize/lines-to-rows spec* filtered-lines)
          rows* (repeat/repeat-down-rows spec* rows)
          rows** (tx/process-rows spec* rows*)
          rows*** (transpose/transpose-rows spec* rows**)
          ]
      (are [x y] (= x y)
           4 (count lines*)
           4 (count filtered-lines)
           4 (count rows*)
           3 (count rows**)
           12 (count rows***)
           "A" (get-value-cell (first rows***) "origin")
           "1" (get-value-cell (first rows***) "destination")
           "A1"  (get-value-cell (first rows***) "position")

           "C" (get-value-cell (last rows***) "origin")
           "4" (get-value-cell (last rows***) "destination")
           "C4"  (get-value-cell (last rows***) "position")))))

(def post-processing-spec
  {:config {:input-separator \,
            :output-separator \^
            :decimal-separator \space
            }

   :tokens [{:index 0 :name "origin" :tx [(tx/trim) (tx/skip-if-equal "origin")]}
            {:index 1 :name "1" :tx (tx/trim)}
            {:index 2 :name "2" :tx (tx/trim)}
            {:index 3 :name "3" :tx (tx/trim)}
            {:index 4 :name "4" :tx (tx/trim)}
           
            ]
   :transpose {:header-name "destination"
               :value-name "position"
               :columns ["1" "2" "3" "4"]}

   :post [{:name "destination" :tx [(tx/convert-to-int)]}]
   
   })


(deftest post-processing
  (testing "post processing some columns"
    (let [spec* (core/create-spec post-processing-spec)
          lines* (input/strings-to-lines (:config spec*) transpose-lines)
          filtered-lines (skip/skip-lines spec* lines*)
          rows (tokenize/lines-to-rows spec* filtered-lines)
          rows* (repeat/repeat-down-rows spec* rows)
          rows** (tx/process-rows spec* rows*)
          rows*** (transpose/transpose-rows spec* rows**)
          rows**** (tx/post-process-rows spec* rows***)
          ]
      (are [x y] (= x y)
           4 (count lines*)
           4 (count filtered-lines)
           4 (count rows*)
           3 (count rows**)
           12 (count rows***)
           "A" (get-value-cell (first rows****) "origin")
           1 (get-value-cell (first rows****) "destination")
           "A1"  (get-value-cell (first rows****) "position")

           "C" (get-value-cell (last rows****) "origin")
           4 (get-value-cell (last rows****) "destination")
           "C4"  (get-value-cell (last rows****) "position")))))
