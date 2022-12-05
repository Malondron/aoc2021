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
    (and (>= (second fc) (first sc) (first fc)  )  (<= (first fc ) (second sc) (second fc))  )
    )
    
    )
  )
 (checkContain (nth (parse-input test-input) 4)) 

(defn solv1 [lines sum]
  (if (= 0 (count lines))
    sum
    (recur (rest lines) (+ sum (if ( checkContain (first lines)) 1 0)))
    )
  )

(solv1 (parse-input test-input) 0)
(solv1 (parse-input (slurp input-file)) 0)

(defn overlap? [pair]
  (let [f (first pair)
        s (second pair)
        fc (map parse-long (str/split f #"-"))
        sc (map parse-long (str/split s #"-"))
        ]


   (or (>= (second sc) (first fc) (first sc)  )  (<= (first sc ) (second fc) (second sc)) 
     (>= (second fc) (first sc) (first fc)  )  (<= (first fc ) (second sc) (second fc))  
    )
     )
    
       
       
    
  )


(defn solv2 [lines sum]
  (if (= 0 (count lines))
    sum
    (recur (rest lines) (+ sum (if ( overlap? (first lines))  1 0)))
    )
  )

 (overlap? (nth (parse-input test-input) 4)) 

(solv2 (parse-input test-input) 0)
(solv2 (parse-input (slurp input-file)) 0)
