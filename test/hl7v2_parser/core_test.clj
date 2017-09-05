(ns hl7v2-parser.core-test
  (:require [clojure.test :refer :all]
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
    (is (= (#'core/escape-character->character "+F+ +E+"
                                               (update default-delimeters :escape-character (constantly "+")))
           "| +")
        "Conversion should work with all escape characters"))
  (testing "Retriving delimeters from message"
    (is (= (#'core/get-delimeters "MSH|^~\\&|") default-delimeters))
    (is (= (:field-delimeter (#'core/get-delimeters "MSH#^~\\&")) "#"))
    (is (= (#'core/get-delimeters "PID") default-delimeters)
        "If MSH header not provided default delimeter should be used")))
