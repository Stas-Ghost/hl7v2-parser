(ns hl7v2-parser.xsd-parser
  "Utilites for parsing XML schemas of HL7 messages"
  (:require
   [clojure.data.xml :as xml]
   [clojure.string :as str]))

(defn xsd->xml [filename]
  (xml/parse (java.io.ByteArrayInputStream. (.getBytes (slurp (clojure.java.io/resource filename))))))
(defn attrs [xml] (->> xml (drop 2) first second rest))

(defn node->field [node]
  (let [[a _ c] node
        name  (->> c :attrs :name)
        attrs (for [el (:content a) :let [attr (:attrs el)]]
                [(keyword (:name attr)) (:fixed attr)])]
    [name (into {} attrs)]))

(def fields (->> (xsd->xml "fields.xsd") attrs (partition 3) (map node->field) (into {})))

(defn node->segment [node]
  (let [[a b] node
        name (->> b :attrs :name)
        attrs (for [el (-> a :content first :content) :let [attr (:attrs el)] :when (not= (:ref attr) nil)]
                [(:ref attr) (dissoc attr :ref)])]
    [name (into {} attrs)]))

(def segments (->> (xsd->xml "segments.xsd") attrs (partition 2) (map node->segment) (into {})))

(defn node->datatype [node]
  (let [[a _ c] node
        name  (->> a :attrs :name)
        attrs (for [el (:content c) :let [attr (:attrs el)]]
                [(keyword (:name attr)) (:fixed attr)])]
    [name (into {} attrs)]))

(def datatypes (->> (xsd->xml "datatypes.xsd") attrs (filter #(< 3 (count (->> % :attrs :name))))
                    (partition 3) (map node->datatype) (into {})))

(def simple-datatypes (->> (xsd->xml "datatypes.xsd")
                           attrs
                           (filter #(= (:tag %) :xmlns.http%3A%2F%2Fwww.w3.org%2F2001%2FXMLSchema/simpleType))
                           (map (comp :name :attrs)) set))

(defn generate-datasets [output-path]
  (doseq [name ["fields" "segments" "datatypes"
                "simple-datatypes"]]
    (clojure.pprint/pprint (var-get (resolve (symbol name)))
                           (clojure.java.io/writer (str output-path name ".clj")))))
