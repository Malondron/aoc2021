(ns prob-3.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(def input-file "C:\\programming\\aoc2021\\prob-3\\input.txt")

(defn countCommon [instrings]
  (for [i (range (count (first instrings)))]
    (let [nms (frequencies (map #(nth % i) instrings))
          ones (or (get nms \1) 0)
          zeroes (or (get nms \0) 0)]
      (if (> ones zeroes)
        [1 0] [0 1]))))

(reduce str (map first (countCommon ["00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000" "11001" "00010" "01010"])))

(defn solve1 [file]
  (let [invals (str/split (slurp file) #"\n")
        ret (countCommon invals)]
    (* (Long/parseLong (reduce str (map first ret)) 2)
       (Long/parseLong (reduce str (map second ret)) 2))))

(solve1 input-file)

(defn findOxy [instrings pos]
  (if (= 1 (count instrings))
    (first instrings)
    (let [nms (frequencies (map #(nth % pos) instrings))
          ones (or (get nms \1) 0)
          zeroes (or (get nms \0) 0)
          new-instrs (filter #(= (if (>= ones zeroes) \1 \0) (nth % pos)) instrings)
          ]
      
      (recur new-instrs (inc pos)))
    )
  )

(findOxy ["00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000" "11001" "00010" "01010"] 0)

(defn findCO2 [instrings pos]
  (if (= 1 (count instrings))
    (first instrings)
    (let [nms (frequencies (map #(nth % pos) instrings))
          ones (or (get nms \1) 0)
          zeroes (or (get nms \0) 0)
          new-instrs (filter #(= (if (>= ones zeroes) \0 \1) (nth % pos)) instrings)
          ]
      
      (recur new-instrs (inc pos)))
    )
  )

(findCO2 ["00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000" "11001" "00010" "01010"] 0)

(defn solve2 [file]
  (let [invals (str/split (slurp file) #"\n")
        oxy (findOxy invals 0)
        co2 (findCO2 invals 0)
        ]
    (* (Long/parseLong (reduce str oxy) 2)
       (Long/parseLong (reduce str co2) 2))))



(solve2 input-file)


(str/split (slurp input-file) #"\n")

