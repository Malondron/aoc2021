(ns src.prob-4.2022-3
  (:require [clojure.string :as str]))


(def input-file "d:\\programming\\aoc2021\\prob-4\\input-2022-3.txt")


(def test-input "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(defn parse-input [inp]
  (let [lines (str/split inp #"\n")]
   lines 
    )
  )

(defn parse-input-2 [inp]
  (let [lines (str/split inp #"\n")]
   (partition 3 lines) 
    )
  )


(parse-input test-input)
(parse-input-2 test-input)

(defn findItem [inp]
  (let [inp-len (count inp)
        fir (subs inp 0 (/ inp-len 2))
        sec (subs inp (/ inp-len 2))
        el (first (for [f fir :when (.contains sec (str f))] f))
        ]
    (if (Character/isLowerCase el) (- (int el) 96)

        (- (int el) 38))
    )
  )



 (findItem (second (parse-input test-input)))
(Character/isUpperCase \a)
(int \L)

(defn solv1 [lines sum ]
  (if (= 0 (count lines))
    sum
    (recur (rest lines) (+ sum (findItem (first lines))))
    
    )
  )

(solv1 (parse-input test-input) 0)
(solv1 (parse-input (slurp input-file)) 0)

 (defn findItem2 [lines]
   (let [fir (first lines)
         sec (second lines)
         thr (nth lines 2)
         fs (for [f fir :when (.contains sec (str f))] f)
         el (first (for [f (str fs) :when (.contains thr (str f))] f))
         ]
    (if (Character/isLowerCase el) (- (int el) 96)

        (- (int el) 38))
     )
   )
 
(findItem2 (second (parse-input-2 test-input)))