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

(defrecord TransposeSpec [^String header-name
                          ^String value-name
                          columns
                          ])

(defrecord Spec [^Config config
                 skip
                 tokens
                 ^TransposeSpec transpose])

(defrecord Line [^Integer index
                 ^String str])

(defrecord Cell [^String name
                 value])

(defrecord Row [^Integer index
                cells])

;;
;; user specification to Spec
;; TODO: use "or" to create default values
;;
(defn- ^Config create-config [config]
  (Config. (:input-separator config \,)
           (:input-quote config \")
           (:output-separator config \^)
           (:thousand-separator config nil)
           (:decimal-separator config nil)))

(defn- create-skipspecs [skip]
  (SkipSpec. skip))

(defn- create-tokenspecs [tokens]
  (map (fn [token]
         (TokenSpec. (:index token)
                     (:name token)
                     (:value token)
                     (:repeat-down token)
                     (:tx token)))
       tokens))

(defn- create-transposespec [transpose]
  (TransposeSpec. (:header-name transpose)
                  (:value-name transpose)
                  (:columns transpose)))

(defn create-spec [spec]
  (Spec. (create-config (:config spec))
                       (create-skipspecs (:skip spec))
                       (create-tokenspecs (:tokens spec))
                       (create-transposespec (:transpose spec))))
