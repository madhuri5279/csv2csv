(ns csv2csv.tx
  (:require [csv2csv.core :as core])
  (:require [csv2csv.util :as util]))

;;
;; build-in cell transformations
;;
(defn skip-if-equal [value]
  (fn [^csv2csv.core.Config config ^csv2csv.core.Cell cell ^csv2csv.core.Row row]
    (if (= value (:value cell))
      :skip-row
      (:value cell))))

(defn convert-to-int []
  (fn [^csv2csv.core.Config config ^csv2csv.core.Cell cell ^csv2csv.core.Row row]
    (if (not (empty? (:value cell)))
      (try
        (let [thousand-separator (:thousand-separator config)
              value (:value cell)]
          (if (empty? thousand-separator)
            (int (Double/parseDouble value))
            (int (Double/parseDouble (clojure.string/replace value thousand-separator "")))))
        (catch Exception e (println (str "Error convert to int: " (:value cell)  (.getMessage e))))
        )
      nil
      )))

(defn convert-to-double []
  (fn [^csv2csv.core.Config config ^csv2csv.core.Cell cell ^csv2csv.core.Row row]
    (if (not (empty? (:value cell)))
      (try
        (let [thousand-separator (:thousand-separator config)
              value (:value cell)]
          (if (empty? thousand-separator)
            (Double/parseDouble value)
            (Double/parseDouble (clojure.string/replace value thousand-separator ""))))
        (catch Exception e (println (str "Error convert to double: " (:value cell)  (.getMessage e)))))
      nil
      )))

(defn trim []
  (fn [^csv2csv.core.Config config ^csv2csv.core.Cell cell ^csv2csv.core.Row row]
    (clojure.string/trim (:value cell))))

(defn copy-from-cell [source-cell-name]
  (fn [^csv2csv.core.Config config ^csv2csv.core.Cell cell ^csv2csv.core.Row row]
    (when-let [source-cell (util/get-cell-with-name source-cell-name row)]
      (:value source-cell))))
;;
;; Cell transformation: [Row] to [Row]
;;
(defn ^csv2csv.core.Cell process-cell [^csv2csv.core.Config config tokens cell ^csv2csv.core.Row row]
  (if-let [token (first (util/get-tokens-with-name tokens (:name cell)))]
    (if (util/is-functions? (:tx token))
      (let [f (util/compose-functions (:tx token))
            value* (f config cell row)]
        (merge cell {:value value*}))
      cell)
    cell
    ))

(defn process-rows [^csv2csv.core.Spec spec rows]
  (let [config (:config spec)
        tokens (:tokens spec)]
    (remove nil?
            (map (fn [row]
                   (let [cells* (map (fn [cell]
                                       (process-cell config tokens cell row))
                                     (:cells row))]
                     (if (some #(= :skip-row (:value %)) cells*)
                       nil
                       (csv2csv.core.Row. (:index row)
                             cells*))
                     ))
                 rows)
            )))


(defn post-process-rows [^csv2csv.core.Spec spec rows]
  (let [config (:config spec)
        tokens (:post spec)]
    (if (or (nil? tokens)
            (empty? tokens))
      rows
      (remove nil?
              (map (fn [row]
                     (let [cells* (map (fn [cell]
                                         (process-cell config tokens cell row))
                                       (:cells row))]
                       (if (some #(= :skip-row (:value %)) cells*)
                         nil
                         (csv2csv.core.Row. (:index row)
                                            cells*))
                       ))
                   rows)
              ))))

