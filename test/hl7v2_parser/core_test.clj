(ns hl7v2-parser.core-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer :all]
   [hl7v2-parser.core :as core :refer :all]))

(def default-delimeters
  {:field-delimeter "|"
   :component-delimeter "^"
   :repetition-delimeter "~"
   :escape-character "\\"
   :subcomponent-delimeter "&"})

(deftest parser-ancillary-functions-test
  (testing "Escape character conversion"
    (is (= (#'core/escape-character->character "" default-delimeters) ""))
    (is (= (#'core/escape-character->character "abcd" default-delimeters) "abcd"))
    (is (= (#'core/escape-character->character "\\F\\ \\R\\ \\S\\ \\T\\ \\E\\ \\P\\" default-delimeters)
           "| ~ ^ & \\ *"))
    (is (= (#'core/escape-character->character
            "+F+ +E+"
            (update default-delimeters :escape-character (constantly "+")))
           "| +")
        "Conversion should work with all escape characters"))
  (testing "Retriving delimeters from message"
    (is (= (#'core/get-delimeters "MSH|^~\\&|") default-delimeters))
    (is (= (:field-delimeter (#'core/get-delimeters "MSH#^~\\&")) "#"))
    (is (= (#'core/get-delimeters "PID") default-delimeters)
        "If MSH header not provided default delimeter should be used")))

(deftest datatypes-test
  (testing "Simple datatypes handling"
    (is (= (#'core/composite->simple "ST" default-delimeters "") '({:name "ST" :value ""})))
    (is (= (#'core/composite->simple "ST" default-delimeters "123") '({:name "ST" :value "123"})))
    (is (= (#'core/composite->simple "ST" default-delimeters "\\F\\") '({:name "ST" :value "|"}))
        "For simplee datatypes escape characters conversion should be done")
    (is (= (#'core/composite->simple "NONE" default-delimeters "123") '())))
  (testing "Complex datatypes handling"
    (is (= (count (#'core/composite->simple "CX" default-delimeters "^^^a")) 4)
        "Empty components should be parsed")
    (is (= (count (#'core/composite->simple "CX" default-delimeters "^^^a^^^^")) 4)
        "Empty trailing components should NOT be parsed")
    (is (= (#'core/composite->simple "HD" default-delimeters "1#2#3" "#")
           '({:name "HD.1", :value ({:name "IS", :value "1"})}
             {:name "HD.2", :value ({:name "ST", :value "2"})}
             {:name "HD.3", :value ({:name "ID", :value "3"})}))
        "Provided delimeter must be used")
    (is (= (#'core/composite->simple
            "CX"
            default-delimeters
            "105431122VA^^^VA STARLIMS_Stage&2.16.840.1.114222.4.3.3.2.2.1&ISO^MR^VA PHL Richmond&2.16.840.1.114222.4.1.9977&ISO")
           '({:name "CX.1", :value ({:name "ST", :value "105431122VA"})}
             {:name "CX.2", :value ({:name "ST", :value ""})}
             {:name "CX.3", :value ({:name "ID", :value ""})}
             {:name "CX.4",
              :value
              ({:name "HD.1", :value ({:name "IS", :value "VA STARLIMS_Stage"})}
               {:name "HD.2",
                :value ({:name "ST", :value "2.16.840.1.114222.4.3.3.2.2.1"})}
               {:name "HD.3", :value ({:name "ID", :value "ISO"})})}
             {:name "CX.5", :value ({:name "ID", :value "MR"})}
             {:name "CX.6",
              :value
              ({:name "HD.1", :value ({:name "IS", :value "VA PHL Richmond"})}
               {:name "HD.2", :value ({:name "ST", :value "2.16.840.1.114222.4.1.9977"})}
               {:name "HD.3", :value ({:name "ID", :value "ISO"})})})))
    ;; TODO How should components with varies datatype parsed? OBX.5 for example
    (is (= (#'core/composite->simple
            "varies"
            default-delimeters
            "VA12345^^^VA STARLIMS Stage&2.16.840.1.114222.4.3.3.2.2.1&ISO")
           '({:name "varies", :value "VA12345"}
             {:name "varies", :value ""}
             {:name "varies", :value ""}
             {:name "varies",
              :value
              "VA STARLIMS Stage&2.16.840.1.114222.4.3.3.2.2.1&ISO"})))))

(deftest parse-segment-test
  (testing "Parsing fields"
    (is (= (#'core/parse-segment ["PID"] default-delimeters) '())
        "Segment name should be absent")
    (is (= (map :name (#'core/parse-segment ["PID" "a" "b" "c"] default-delimeters)) '("PID.1" "PID.2" "PID.3"))
        "Fields ids should begin from 1")
    (is (= (map :name (#'core/parse-segment ["PID" "" "" "c"] default-delimeters)) '("PID.1" "PID.2" "PID.3"))
        "Empty fields should be preserved")
    (is (= (#'core/parse-segment ["NONE" "a" "" "c"] default-delimeters)
           '({:name "NONE.1", :value "a"}
             {:name "NONE.2", :value ""}
             {:name "NONE.3", :value "c"}))
        "If field is unknown or empty it's value should left unprocessed")
    (testing "Repetition of fields"
      (with-redefs [hl7v2-parser.datasets.segments/segments
                    {"TEST" {"TEST.1" {:minOccurs "0", :maxOccurs "1"}
                             "TEST.2" {:minOccurs "0", :maxOccurs "unbounded"}
                             "TEST.3" {:minOccurs "0", :maxOccurs "2"}}}
                    hl7v2-parser.datasets.fields/fields
                    {"TEST.1" {:Type "ST"}
                     "TEST.2" {:Type "ST"}
                     "TEST.3" {:Type "ST"}}]
        (let [r (->> (#'core/parse-segment ["TEST" "1~1"] default-delimeters) last :value)]
          (is (= r '({:name "ST", :value "1~1"})))
          (is (= (count r) 1) "Fields with cardinality 1 should not be splited by repetition delimeter")
          (is (-> r first :value (str/includes? "~")) "Delimeter is unprocessed"))
        (let [r (->> (#'core/parse-segment ["TEST" "" "2~2"] default-delimeters) last :value)]
          (is (= r '[({:name "ST", :value "2"}) ({:name "ST", :value "2"})]))
          (is (= (count r) 2) "Fields with unbounded cardinality should be splited by repetition delimeter"))
        (let [r (->> (#'core/parse-segment ["TEST" "" "" "3~3"] default-delimeters) last :value)]
          (is (= r '[({:name "ST", :value "3"}) ({:name "ST", :value "3"})]))
          (is (= (count r) 2) "Fields with cardinality bigger than 1 should be splited by repetition delimeter"))))))
