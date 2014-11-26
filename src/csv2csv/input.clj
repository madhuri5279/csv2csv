(ns csv2csv.input
  (:require [csv2csv.core :as core])
  (:require [dk.ative.docjure.spreadsheet :as xls])
  (:require [clojure.data.csv :as csv])
  (:require [clojure.tools.logging :as log])
  (:require [clojure.stacktrace])
  (:import (org.apache.poi.ss.usermodel Row Cell DataFormatter DateUtil)))

;;
;; READ LINES FROM CSV/XLS/XLSX
;;
(defn- xls-row-to-tokens [^org.apache.poi.ss.usermodel.Row row]
  (->> (map #(.getCell row % Row/RETURN_BLANK_AS_NULL) (range 0 (.getLastCellNum row)))
       (map #(if (nil? %) "" (xls/read-cell %)))))

(defn- xls-row-to-line [^org.apache.poi.ss.usermodel.Row row]
  (->> row
       (xls-row-to-tokens)
;;       (filter #(not (nil? %)))
       (clojure.string/join (char 31))))
;;
;; multi method row-to-string
;;
(defmulti row-to-string (fn [data] (class data)))
(defmethod row-to-string java.lang.String [data]
  (str data))
(defmethod row-to-string org.apache.poi.hssf.usermodel.HSSFRow [data]
  (xls-row-to-line data))
(defmethod row-to-string org.apache.poi.xssf.usermodel.XSSFRow [data]
  (xls-row-to-line data))
(defmethod row-to-string :default [data]
  "")

(defn- report-all-sheetnames [workbook]
  (->> (xls/sheet-seq workbook)
       (map #(xls/sheet-name %))))

(defn- throw-exception-invalid-sheetname! [workbook sheetname]
  (throw (Exception. (str "Unable to find sheet with name " sheetname ". "
                          "List of available sheets: " 
                          (apply str (interpose "," (report-all-sheetnames workbook)))))))

(defn- find-sheet-with-name [workbook sheetname]
  (if (or (nil? sheetname)
          (empty? sheetname))
    (throw-exception-invalid-sheetname! workbook sheetname)
    (let [pattern (re-pattern sheetname)
          sheet (xls/select-sheet pattern workbook)]
      (if (nil? sheet)
        ;; dump a list of available sheetnames and throw an exception
        (throw-exception-invalid-sheetname! workbook sheetname)
        sheet)
      )))

(defn lazy-file-lines [filename]
  (letfn [(helper [rdr]
            (lazy-seq
             (if-let [line (.readLine rdr)]
               (cons line (helper rdr))
               (do (.close rdr) nil))))]
    (helper (clojure.java.io/reader filename))))

(defn- get-file-extension [params]
  (let [filename (:filename params)
        extension (last (clojure.string/split filename #"\."))]
    (cond
     (= extension "xlsx") :xlsx
     (= extension "xls")  :xlsx
     (= extension "csv")  :csv
     (= extension "tsv")  :csv
     (= extension "txt")  :csv
     :else :error ;;TODO: replace with log and exception
     )))

(defmulti read-lines
  (fn [params]
    (get-file-extension params)))

(defmethod read-lines :csv [{:keys [filename max]}]
  (if (nil? max)
    (lazy-file-lines filename)
    (take max (lazy-file-lines filename))))

(defmethod read-lines :xls [{:keys [filename sheetname max]}]
  (let [workbook (xls/load-workbook filename)
        sheet (find-sheet-with-name workbook sheetname)
        lines (xls/row-seq sheet)
        lines* (map #(row-to-string %) lines)
        ]
    (if (nil? max)
      lines*
      (take max lines*))))

(defmethod read-lines :xlsx [params]
  (let [{:keys [filename sheetname max]} params
        workbook (xls/load-workbook filename)
        sheet (find-sheet-with-name workbook sheetname)
        lines (xls/row-seq sheet)
        lines* (map #(row-to-string %) lines)
        ]
    (if (nil? max)
      lines*
      (take max lines*))))

(defmethod read-lines :default [& args]
  [])
;;
;; read file (csv,tsv,txt,xls,xlsx) and return list of strings: IO -> [string]
;;
(defn read-file
  ([filename]
     (read-lines {:filename filename :sheetname nil :max nil}))
  ([filename sheetname]
     (read-lines {:filename filename :sheetname sheetname :max nil}))
  ([filename sheetname max]
     (read-lines {:filename filename :sheetname sheetname :max max})))
;;
;; read input strings into Lines: [string] to [Line]
;; 
(defn strings-to-lines [^csv2csv.core.Config config strings]
  (map (fn [index line]
         (csv2csv.core.Line. index line)) (range) strings))
