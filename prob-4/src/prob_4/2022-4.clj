(ns src.prob-4.2022-4
  (:require [clojure.string :as str]))



(def input-file "d:\\programming\\aoc2021\\prob-4\\input-2022-4.txt")

(def test-input "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")


(defn parse-input [inp]
  (let [lines (str/split inp #"\n")]
    (mapv #(str/split % #",") lines)
    )
  )

(parse-input test-input)

(defn checkContain [pair]
  (let [f (first pair)
        s (second pair)
        fc (map parse-long (str/split f #"-"))
        sc (map parse-long (str/split s #"-"))
        ]
(or (and (>= (second sc) (first fc) (first sc)  )  (<= (first sc ) (second fc) (second sc))  )
    (and (>= (first sc) (first fc))  (>= (second sc) (second fc))  ))
    
    )
  )
 (checkContain (first (parse-input test-input))) 