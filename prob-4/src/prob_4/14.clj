(ns src.prob-4.14
  (:require [clojure.string :as str]))

(def input-file "d:\\programming\\aoc2021\\prob-4\\input-14.txt")

(def test-input "NNCB

H -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")

(str/split test-input #"\n\n")

(defn parse-input [inp]
  (let [m&i (str/split inp #"\n\n")
        ms  (mapv #(str/split % #" -> ") (str/split (second m&i) #"\n"))
        is  (first m&i) 
        ]
    
    [is ms]
    ))

(parse-input test-input)

(defn polymer [el pols]
  (let [found (filter #(= el (first %)) pols)]
    (str (first el) (second (first found)) )
    )
  )

(defn makePolStep [start pols]
  (let [pairs (for [i (range (dec (count start)))]
                (str (nth start i) (nth start (inc i)))
                )]
    (str (apply str (map #(polymer % pols) pairs) ) (last (last pairs)))
    
    )
  )


(defn makeNsteps [steps step start pols]
  (if (= step steps)
    start
    (recur steps (inc step) (makePolStep start pols) pols)
    )
  )

(makePolStep (first (parse-input test-input)) (second (parse-input test-input)))

(defn solv1 [input]
  
 (let [inp (parse-input input)
       after-poly (makeNsteps 10 0 (first inp) (second inp))
       freqs (frequencies after-poly)
       vals (vals freqs)
       min-val (apply min vals)
       max-val (apply max vals)
       
       ]
(- max-val min-val)
  )
  )

(solv1 test-input)
(solv1 (slurp input-file))