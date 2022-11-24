(ns src.prob-4.7
  
  (:require [clojure.string :as str])
  
  )

(defn parse-input [file]
  (mapv parse-long (str/split (str/trim (slurp file)) #","))
  )

(parse-input input-file)

(def input-file "d:\\programming\\aoc2021\\prob-4\\input-7.txt")


(defn fuelTo [poses posTo]
  (reduce + (for [pos poses]
              (abs (- pos posTo))
              )))

(fuelTo [16 1 2 0 4 2 7 1 2 14] 2)

(defn cheapestFuel [poses]
  (let [from (apply min poses)
        to (apply max poses)
        testPoses (range from (inc to))
        ]
    (apply min (map #(fuelTo poses %) testPoses)) 
    ))



(cheapestFuel [16 1 2 0 4 2 7 1 2 14])
(cheapestFuel (parse-input input-file))

(defn fuelTo2 [poses posTo]
  (reduce + (for [pos poses]
              (let [n (abs (- pos posTo))]
               (/ (* n (inc n)) 2) 
                )
              )))

(defn cheapestFuel2 [poses]
  (let [from (apply min poses)
        to (apply max poses)
        testPoses (range from (inc to))
        ]
    (apply min (map #(fuelTo2 poses %) testPoses)) 
    ))

(cheapestFuel2 (parse-input input-file))