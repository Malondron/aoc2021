(ns src.prob-4.2022-2
  (:require [clojure.string :as str]))

(def input-file "d:\\programming\\aoc2021\\prob-4\\input-2022-2.txt")

(def test-input "A Y
B X
C Z")


(defn parse-input [inp]
  (mapv #(str/split % #" ") (str/split inp #"\n"))
  )



(defn calcRoundPoints [round]
  (let [opp (first round)
        me (second round)
        ]
    (case opp
      "A" (case me
            "X" (+ 3 1)
            "Y" (+ 6 2)
            "Z" (+ 0 3))
      "B" (case me
            "X" (+ 0 1)
            "Y" (+ 3 2)
            "Z" (+ 6 3))
       "C" (case me
            "X" (+ 6 1)
            "Y" (+ 0 2)
            "Z" (+ 3 3))
            
      )
    
    )
  )
