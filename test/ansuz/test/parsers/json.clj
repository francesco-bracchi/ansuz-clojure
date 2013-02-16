(ns ansuz.test.parsers.json
  (:use [ansuz.language])
  (:use [ansuz.parsers.json])
  (:use [clojure.test]))

(def astring "{
    \"glossary\": {
        \"title\": \"example glossary\",
		\"GlossDiv\": {
            \"title\": \"S\",
			\"GlossList\": {
                \"GlossEntry\": {
                    \"ID\": \"SGML\",
					\"SortAs\": \"SGML\",
					\"GlossTerm\": \"Standard Generalized Markup Language\",
					\"Acronym\": \"SGML\",
					\"Abbrev\": \"ISO 8879:1986\",
					\"GlossDef\": {
                        \"para\": \"A meta-markup language, used to create markup languages such as DocBook.\",
						\"GlossSeeAlso\": [\"GML\", \"XML\"]
                    },
					\"GlossSee\": \"markup\"
                }
            }
        }
    }
}
")

(def astruct 
  {:glossary
   {:GlossDiv
    {:GlossList
     {:GlossEntry
      {:GlossSee "markup",
       :GlossDef
       {:GlossSeeAlso ["GML" "XML"],
        :para
        "A meta-markup language, used to create markup languages such as DocBook."},
       :Abbrev "ISO 8879:1986",
       :Acronym "SGML",
       :GlossTerm "Standard Generalized Markup Language",
       :SortAs "SGML",
       :ID "SGML"}},
     :title "S"},
    :title "example glossary"}})

(deftest test-parse
  (is (.equals (parse astring) astruct)))

