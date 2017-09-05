(ns hl7v2-parser.core
  "Provides functions for parsing HL7 messages"
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [hl7v2-parser.datasets.datatypes :refer [datatypes]]
   [hl7v2-parser.datasets.segments :refer [segments]]
   [hl7v2-parser.datasets.fields :refer [fields]]
   [hl7v2-parser.datasets.simple-datatypes :refer [simple-datatypes]]))

(defn- escape-character->character
  [in {:keys [field-delimeter
              component-delimeter
              repetition-delimeter
              escape-character
              subcomponent-delimeter]}]
  (str/replace in
               (re-pattern
                (str
                 (java.util.regex.Pattern/quote escape-character)
                 "\\w{1}"
                 (java.util.regex.Pattern/quote escape-character)))
               #(condp = %
                  (str escape-character "F" escape-character) field-delimeter
                  (str escape-character "R" escape-character) repetition-delimeter
                  (str escape-character "S" escape-character) component-delimeter
                  (str escape-character "T" escape-character) subcomponent-delimeter
                  (str escape-character "E" escape-character) escape-character
                  (str escape-character "P" escape-character) "*"
                  (str escape-character "H" escape-character) "" ;; start highlighting
                  (str escape-character "N" escape-character) "" ;; stop highlighting
                  %)))

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

(defn- split-by-delimeter [s d]
  (str/split s (re-pattern (java.util.regex.Pattern/quote d))))

(defn- composite->simple
  ([datatype {:keys [component-delimeter] :as delimeters} value]
   (composite->simple datatype delimeters value component-delimeter))
  ([datatype {:keys [subcomponent-delimeter] :as delimeters} value delimeter]
   (for [component (map-indexed vector (split-by-delimeter value delimeter))
         :let [[k v] component
               datatype-component-name (str datatype "." (inc k))
               datatype-data (get datatypes datatype-component-name false)]
         :when (or datatype-data
                   (= "varies" datatype)
                   (simple-datatypes datatype))]
     (if datatype-data
       {:name datatype-component-name
        :value (composite->simple (:Type datatype-data) delimeters v subcomponent-delimeter)}
       {:name datatype
        :value (escape-character->character v delimeters)}))))

(defn- parse-segment [segment {:keys [repetition-delimeter] :as delimeters}]
  (let [segment-name (first segment)
        segment-data (get segments segment-name {})]
    (for [field (map vector (range 1 (count segment)) (rest segment))
          :let [[k v] field
                field-name (str segment-name "." k)
                field-data (get fields field-name false)
                field-repeatable? (not= "1" (get-in segment-data [field-name :maxOccurs] "1"))
                field-empty? (or (= "" v) (false? field-data))
                field-type (get field-data :Type)
                value (cond
                        field-empty? ""
                        field-repeatable? (mapv (partial composite->simple field-type delimeters)
                                                (split-by-delimeter v repetition-delimeter))
                        :else (composite->simple field-type delimeters v))]]
      {:name field-name
       :value value})))

(defn parse-message [s]
  (try
    (let [delimeters (get-delimeters s)
          {:keys [field-delimeter]} delimeters
          ;; fix for MSH.1 field - field separator
          s-fixed (str/replace s (str "MSH" field-delimeter) (str "MSH" field-delimeter field-delimeter))]
      (for [segment-s (str/split s-fixed #"[\n\r]")
            :let [segment (split-by-delimeter segment-s field-delimeter)]]
        {:name (first segment)
         :value (parse-segment segment delimeters)}))
    (catch Exception e
      (str "caught exception: " (.getMessage e)))))
