# hl7v2-parser

[![Clojars Project](https://img.shields.io/clojars/v/hl7v2-parser.svg)](https://clojars.org/hl7v2-parser)
[![CircleCI](https://circleci.com/gh/Stas-Ghost/hl7v2-parser.svg?style=svg)](https://circleci.com/gh/Stas-Ghost/hl7v2-parser)

## Usage 

```clojure
(require '[hl7v2-parser.core :as parser])
(parser/parse-message 
(slurp "http://pathology.healthbase.info/viewer/samples/Syndromic%20Surveillance%20(US)/PHLIP%20influenza%20R1.0.2.hl7"))

```

## License

Copyright © 2017 Stanislav Yakushev

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
