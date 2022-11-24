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

(def countFish 
 (memoize
  (fn [days fish-timer]
    (->> (iterate #(- % 7) (- days (inc fish-timer)))
         (take-while (complement neg?))
         (map #(countFish % 8)) 
         (reduce + 1)
         ))))


(def countFishMem (memoize countFish))
  
  

  (countFish 8 3))


(last (str/split (str/trim (slurp input-file)) #",")))

(->>  (mapv parse-long (str/split (str/trim (slurp input-file)) #","))
     (map (partial countFish 256))
     (reduce +)
     )
(count (simDays 18 [3 4 3 1 2]))
(* 2 (Math/pow 2 (Math/floor (/ 7 9))))
(* 2 (Math/pow 2 (Math/floor (/ 7 7))))
(slurp input-file)
(count (simDays 80 (map #(Integer/parseInt %) (str/split (str/trim (slurp input-file)) #","))))