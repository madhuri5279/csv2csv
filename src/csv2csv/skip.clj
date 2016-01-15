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
;; start of lines
;;
(defn start-at-lines
  ([^csv2csv.core.Spec spec lines]
     (start-at-lines (:config spec) (:start spec) lines))
  ([^csv2csv.core.Config config ^csv2csv.core.StartSpec startspec lines]
     (if (util/is-functions? (:fn startspec))
       (let [f (util/compose-skip-functions (:fn startspec))]
         (drop-while #(not (f config %)) lines))
       lines
       )))

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

;;
;; stop processing the lines once a :stop function becomes true
;;
(defn stop-lines
  ([^csv2csv.core.Spec spec lines]
     (stop-lines (:config spec) (:stop spec) lines))
  ([^csv2csv.core.Spec config ^csv2csv.core.StopSpec stopspec lines]
     (if (util/is-functions? (:fn stopspec))
       (let [f (util/compose-stop-functions (:fn stopspec))]
         (take-while #(f config %) lines))
       lines
       )))
