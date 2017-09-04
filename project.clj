(defproject hl7v2-parser "0.1.0-SNAPSHOT"
  :description "Parser for Health Level-7 Version 2 messages"
  :url "https://github.com/Stas-Ghost/hl7v2-parser"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.xml "0.2.0-alpha2"]]
  :main ^:skip-aot hl7v2-parser.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
