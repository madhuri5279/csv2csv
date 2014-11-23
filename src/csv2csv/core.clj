(ns csv2csv.core
  (:gen-class))

;; macro from https://github.com/mjul/docjure
(defmacro assert-type [value expected-type]
  `(when-not (isa? (class ~value) ~expected-type)
     (throw (IllegalArgumentException.    
             (format "%s is invalid. Expected %s. Actual type %s, value: %s"
                     (str '~value) ~expected-type (class ~value) ~value)))))

(defrecord Config [^Character input-separator
                   ^Character input-quote
                   ^Character output-separator
                   ^Character thousand-separator
                   ^Character decimal-separator
                   ])

(defrecord SkipSpec [fn])

(defrecord TokenSpec [^Integer index
                      ^String name
                      value
                      ^Boolean repeat-down
                      tx])

(defrecord Spec [^Config config
                 skip
                 tokens])

(defrecord Line [^Integer index
                 ^String str])

(defrecord Cell [^String name
                 value])

(defrecord Row [^Integer index
                cells])

;;
;; helper functions
;;
(defn compose-skip-functions [fs]
  (if (vector? fs)
    (fn [^Config config ^Line line]
      (loop [value ((first fs) config line) ;; check for special values
             fs (next fs) ]
        (if (nil? fs)
          value
          (recur (or value ((first fs) config line)) (next fs)))
        ))
    (fn [^Config config ^Line line]
      (fs config line))
    ))

(defn compose-functions [fs]
  (if (vector? fs)
    (fn [^Config config ^Cell cell ^Row row]
      (loop [value ((first fs) config cell row) ;; check for special values
             fs (next fs) ]
        (if (nil? fs)
            value
            (recur ((first fs) config (merge cell {:value value}) row) (next fs)))
        ))
    (fn [^Config config ^Cell cell ^Row row]
      (fs config cell row))
    ))

(defn is-functions? [fs]
  (or (fn? fs)
      (and (vector? fs)
           (every? fn? fs))))
;;
;; user specification to Spec
;; TODO: use "or" to create default values
;;
(defn ^Config create-config [config]
  (Config. (:input-separator config)
           (:input-quote config)
           (:output-separator config)
           (:thousand-sepatator config)
           (:decimal-separator config)))

(defn create-skipspecs [skip]
  (SkipSpec. skip))

(defn create-tokenspecs [tokens]
  (map (fn [token]
         (TokenSpec. (:index token)
                     (:name token)
                     (:value token)
                     (:repeat-down token)
                     (:tx token)))
       tokens))

(defn create-spec [spec]
  (Spec. (create-config (:config spec))
         (create-skipspecs (:skip spec))
         (create-tokenspecs (:tokens spec))))
;;
;; read input strings into Lines: [string] to [Line]
;; 
(defn strings-to-lines [^Config config strings]
  (map (fn [index line]
         (Line. index line)) (range) strings))
;;
;; remove Lines using user defined predicates: [Line] to [Line]
;;
(defn skip-lines
  ([^Spec spec lines]
     (skip-lines (:config spec) (:skip spec) lines))
  ([^Config config ^SkipSpec skipspec lines]
     (if (is-functions? (:fn skipspec))
       (let [f (compose-skip-functions (:fn skipspec))]
         (remove #(f config %) lines))
       lines
       )))
;;
;; parse Lines into Rows and Cells: [Line] to [Row]
;;
(defn get-tokens-with-index [tokens index]
  (filter #(= index (:index %)) tokens))

(defn get-tokens-with-name [tokens name]
  (filter #(= name (:name %)) tokens))

(defn safe-nth [v index]
  (if (> (count v) index)
    (nth v index)
    nil))

(defn create-cells [splits tokens]
  (map (fn [^TokenSpec token]
         (assert-type token TokenSpec)
         (if-let [index (:index token)]
           (if-let [split (safe-nth splits index)]
             (Cell. (:name token)
                    split)
             (Cell. (:name token)
                    (:value token)))
           (Cell. (:name token)
                  (:value token))
           )
         )
       tokens))

(defn lines-to-rows 
  ([^Spec spec lines]
     (lines-to-rows (:config spec) (:tokens spec) lines))
  ([^Config config tokens lines]
     (let [re-split (re-pattern (str (:input-separator config)))]
       (map (fn [^Line line]
              (let [splits (clojure.string/split (:str line) re-split -1)] ;;negative limit to avoid omitting the trailing empty strings
                (Row. (:index line)
                      (create-cells splits tokens))))
            lines))))
;;
;; Repeat-down functionality: [Row] to [Row]
;;
(defn repeat-down-cells [^Spec spec ^Cell prev ^Cell curr]
  (if-let [token (first (get-tokens-with-name (:tokens spec) (:name curr)))]
    (if-let [fn-repeat (:repeat-down token)]
      (if (fn-repeat (:config spec) curr)
        (merge curr {:value (:value prev)})
        curr)
      curr)
    curr))

(defn repeat-down-row [^Spec spec ^Row prev ^Row curr]
  (if (nil? prev)
    curr
    (Row. (:index curr)
          (map (fn [prev-cell curr-cell]
                 (repeat-down-cells spec prev-cell curr-cell))
               (:cells prev)
               (:cells curr)))))

(defn repeat? [^Spec spec]
  (some #(not (nil? (:repeat-down %))) (:tokens spec)))

(defn repeat-down-rows
  ([^Spec spec rows]
     (if (repeat? spec)
       (repeat-down-rows spec nil rows)
       rows))
  ([^Spec spec ^Row prev rows]
     (lazy-seq
      (when-let [s (seq rows)]
        (let [row* (repeat-down-row spec prev (first rows))]
          (cons row*
                (repeat-down-rows spec row* (rest rows))))))))
;;
;; Cell transformation: [Row] to [Row]
;;
(defn ^Cell process-cell [^Config config tokens cell ^Row row]
  (if-let [token (first (get-tokens-with-name tokens (:name cell)))]
    (if (is-functions? (:tx token))
      (let [f (compose-functions (:tx token))
            value* (f config cell row)]
        (merge cell {:value value*}))
      cell)
    cell
    ))

(defn process-rows [^Spec spec rows]
  (let [config (:config spec)
        tokens (:tokens spec)]
    (remove nil?
            (map (fn [row]
                   (let [cells* (map (fn [cell]
                                       (process-cell config tokens cell row))
                                     (:cells row))]
                     (if (some #(= :skip-row (:value %)) cells*)
                       nil
                       (Row. (:index row)
                             cells*))
                     ))
                 rows)
            )))

;;
;; build-in skip predicates
;;
(defn line-contains? [text]
  (let [re (if (string? text)
             (re-pattern text)
             text)]
    (fn [^Config config ^Line line]
      (not (nil? (re-find re (:str line)))))))

(defn line-empty? []
  (fn [^Config config ^Line line]
    (or (nil? (:str line))
        (empty? (clojure.string/trim (:str line))))))
;;
;; build-in repeat-down function
;;
(defn empty-cell? []
  (fn [^Config config ^Cell cell]
    (empty? (clojure.string/trim (:value cell)))))
;;
;; build-in cell transformations
;;
(defn convert-to-int []
  (fn [^Config config ^Cell cell ^Row row]
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

(defn trim []
  (fn [^Config config ^Cell cell ^Row row]
    (clojure.string/trim (:value cell))))
;;
;; convention: if returned value is :skip-row then the row will be skipped
;;
(defn skip-if-equal [value]
  (fn [^Config config ^Cell cell ^Row row]
    (if (= value (:value cell))
      :skip-row
      (:value cell))))
