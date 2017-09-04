(ns hl7v2-parser.core
  "Provides functions for parsing HL7 messages"
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]))

(def datatypes-map (edn/read-string (slurp "src/hl7v2_parser/datasets/datatypes.clj")))
(def segments-map (edn/read-string (slurp "src/hl7v2_parser/datasets/segments.clj")))
(def fields-map (edn/read-string (slurp "src/hl7v2_parser/datasets/fields.clj")))
(def simple-datatypes (edn/read-string (slurp "src/hl7v2_parser/datasets/simple-datatypes.clj")))

(defn- escape-character->character
  [in {:keys [field-delimeter
              component-delimeter
              repetition-delimeter
              escape-characer
              subcomponent-delimeter]}]
  (str/replace in #"\\\w+?\\"
               #(condp = %
                  (str escape-characer "F" escape-characer) field-delimeter
                  (str escape-characer "R" escape-characer) repetition-delimeter
                  (str escape-characer "S" escape-characer) component-delimeter
                  (str escape-characer "T" escape-characer) subcomponent-delimeter
                  (str escape-characer "E" escape-characer) escape-characer
                  (str escape-characer "P" escape-characer) "*"
                  (str escape-characer "H" escape-characer) "" ;; start highlighting
                  (str escape-characer "N" escape-characer) "" ;; stop highlighting
                  )))

(defn- get-delimeters [s]
  (if (str/starts-with? s "MSH")
    {:field-delimeter (subs s 3 4)
     :component-delimeter (subs s 4 5)
     :repetition-delimeter (subs s 5 6)
     :escape-character (subs s 6 7)
     :subcomponent-delimeter (subs s 7 8)}
    {:field-delimeter "|"
     :component-delimeter "^"
     :repetition-delimeter "~"
     :escape-character "\\"
     :subcomponent-delimeter "&"}))

(defn- unpack-seq [xs]
  (let [fv (first xs)]
    (if (string? fv) fv xs)))

(defn- composite->simple
  ([datatype {:keys [component-delimeter] :as delimeters} value]
   (composite->simple datatype delimeters value component-delimeter))
  ([datatype {:keys [subcomponent-delimeter] :as delimeters} value delimeter]
   (for [component (map-indexed
                    vector
                    (str/split
                     value
                     (re-pattern (java.util.regex.Pattern/quote delimeter))))
         :let [[k v] component
               datatype-component-name (str datatype "." (inc k))
               datatype-data (get datatypes-map datatype-component-name false)]
         :when (and
                (not= "" v)
                (or datatype-data
                    (simple-datatypes datatype)
                    (= "varies" datatype)))]
     (if datatype-data
       {:name (:Type datatype-data)
        :value (unpack-seq (composite->simple (:Type datatype-data) delimeters v subcomponent-delimeter))}
       (escape-character->character v delimeters)))))

(defn- parse-segment [segment {:keys [repetition-delimeter] :as delimeters}]
  (let [segment-name (first segment)
        segment-data (get segments-map segment-name {})
        repetition-delimeter-pattern (re-pattern (java.util.regex.Pattern/quote repetition-delimeter))]
    (for [field (map vector (range 1 (count segment)) (rest segment))
          :let [[k v] field
                field-name (str segment-name "." k)
                field-data (get fields-map field-name false)
                field-repeatable? (not= "1" (get-in segment-data [field-name :maxOccurs] "1"))
                field-empty? (or (= "" v) (= false field-data))
                field-type (get field-data :Type)
                value (cond
                        field-empty? ""
                        field-repeatable? (mapv (partial composite->simple field-type delimeters)
                                                (str/split v repetition-delimeter-pattern))
                        :else (composite->simple field-type delimeters v))]]
      {:name field-name
       :value value})))

(defn parse-message [s]
  (try
    (let [delimeters (get-delimeters s)
          {:keys [field-delimeter]} delimeters
          ;; fix for MSH.1 field - field separator
          s-fixed (str/replace s (str "MSH" field-delimeter) (str "MSH" field-delimeter field-delimeter))
          field-delimeter-pattern (re-pattern (java.util.regex.Pattern/quote field-delimeter))]
      (for [segment-s (str/split s-fixed #"[\n\r]")
            :let [segment (str/split (str/trim segment-s) field-delimeter-pattern)]]
        {:name (first segment)
         :value (parse-segment delimeters)}))
    (catch Exception e
      (str "caught exception: " (.getMessage e)))))
