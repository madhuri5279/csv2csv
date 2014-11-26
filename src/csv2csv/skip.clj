(ns csv2csv.skip
  (:require [csv2csv.util :as util]))

;;
;; build-in skip predicates
;;
(defn line-contains? [text]
  (let [re (if (string? text)
             (re-pattern text)
             text)]
    (fn [^csv2csv.core.Config config ^csv2csv.core.Line line]
      (not (nil? (re-find re (:str line)))))))

(defn line-empty? []
  (fn [^csv2csv.core.Config config ^csv2csv.core.Line line]
    (or (nil? (:str line))
        (empty? (clojure.string/trim (:str line))))))

;;
;; remove Lines using user defined predicates: [Line] to [Line]
;;
(defn skip-lines
  ([^csv2csv.core.Spec spec lines]
     (skip-lines (:config spec) (:skip spec) lines))
  ([^csv2csv.core.Config config ^csv2csv.core.SkipSpec skipspec lines]
     (if (util/is-functions? (:fn skipspec))
       (let [f (util/compose-skip-functions (:fn skipspec))]
         (remove #(f config %) lines))
       lines
       )))
