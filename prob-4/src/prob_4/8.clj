(ns src.prob-4.8

  (:require [clojure.string :as str])
  )


(def input-file "d:\\programming\\aoc2021\\prob-4\\input-8.txt")

(str/split (str/trim (second (str/split (first (str/split (slurp input-file) #"\n")) #"\|") )) #" ") 

(defn parse-input [file]
  (mapv #(str/split (str/trim (second (str/split % #"\|"))) #" ") (str/split (slurp file) #"\n"))
  )
(parse-input input-file)
(def test-string "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf" )



(defn countLine [line]
  (let [lens (map count line)
        founds (filter #(.contains [2 4 3 7] %) lens)]
    (count founds )))


(countLine ["fdgacbe" "cefdb" "cefbgd" "gcbe"])

(defn count1478 [inlines cnt]
  (if (= 0 (count inlines))
    cnt
    (let [lncnt (countLine (first inlines))]
      
      (recur (rest inlines) (+ lncnt cnt))
      )
    )
  )

(count1478 (parse-input input-file) 0)