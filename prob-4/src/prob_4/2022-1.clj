(ns src.prob-4.2022-1
  
  (:require [clojure.string :as str]))


(def input-file "d:\\programming\\aoc2021\\prob-4\\input-2022-1.txt")

(def test-input "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")


(defn parse-input [inp]
  (let [elves (str/split inp #"\n\n")]
    (mapv #(mapv parse-long (str/split % #"\n")) elves)
    )
  )

(parse-input test-input)

(defn solv1 [elves]
  (->>
   elves
   (map #(reduce + %))
   (apply max))
  ;(apply max (map #(reduce + %) elves))
  )
(solv1 (parse-input test-input))
(solv1 (parse-input (slurp input-file)))

(defn solv2 [elves]
  (->>
   elves
   (map #(reduce + %))
   (sort >)
   (take 3)
   (reduce +)
   )
  ; (reduce + (take 3 (sort > (map #(reduce + %) elves))))
  )
(solv2 (parse-input test-input))
(solv2 (parse-input (slurp input-file)))
