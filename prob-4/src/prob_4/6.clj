(ns src.prob-4.6
  (:require [clojure.string :as str])
  )



(def input-file "d:\\programming\\aoc2021\\prob-4\\input-6.txt")

(defn updateTime [val]
  (if (= val 0)
    6
    (- val 1)))

(defn simDays [day fishes]
  (if (= 0 day)
    fishes
    (let [numbToBirth (count (filter #(= 0 %) fishes))
          new-fishes (take numbToBirth (repeat 8))
          ]
      (recur (- day 1) (concat (map updateTime fishes) new-fishes))
      
      )
    
    )
  )

(count (simDays 18 [0 0 ]))
(* 2 (Math/pow 2 (Math/floor (/ 7 9))))
(* 2 (Math/pow 2 (Math/floor (/ 7 7))))
(slurp input-file)
(count (simDays 80 (map #(Integer/parseInt %) (str/split (str/trim (slurp input-file)) #","))))