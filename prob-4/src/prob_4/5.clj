(ns src.prob-4.5
  
  (:require [clojure.string :as str])
  
  )


(def input-file "d:\\programming\\aoc2021\\prob-4\\input-5.txt")

(slurp input-file)


(defn isHorVert? [x y]
  (or (= (nth x 0) (nth y 0)) (= (nth x 1) (nth y 1))))

(defn new-points [line]
  (when  (isHorVert? (first line) (second line))
          (let [fp (first line)
                sp (second line)
                x0 (first fp)
                y0 (second fp)
                x1 (first sp)
                y1 (second sp)
                ]

            (for [i (range (min x0 x1) (+ 1 (max x0 x1)))]
              (for [j (range (min y0 y1) (+ 1 (max y0 y1)))]

                         [i j])))))

(new-points [[0 9] [5 9]])

(defn collPoints [in-lines outlist]
  (if (= 0 (count in-lines))
    outlist
    (let [new-points (new-points (first in-lines))]
      
      (recur (rest in-lines) (apply conj outlist new-points)))
    )
  )


(defn parseLineLine [inLine]
  (let [parts (str/split inLine #" -> ")]
    
    (for [part parts]
      (let [xy (str/split part #",")]
        [(Integer/parseInt (first xy)) (Integer/parseInt (second xy)) ])
      )
    )
  )

(defn solve1 [file]
  (let [inList (str/split (slurp input-file) #"\n")
        lines (map parseLineLine inList)
        ]

    (->> (collPoints [[[0 9] [5 9]] [[8 0] [0 8]]  [[9 4] [3 4]] [[2 2] [2 1]] [[7 0] [7 4]] [[6 4] [2 0]] [[0 9] [2 9]] [[3 4] [1 4]] [[0 0] [8 8]] [[5 5] [8 2]]] [])
         flatten
         (partition 2)
         frequencies
         vals
         (filter #(> % 1))
         count)))

(parseLineLine (first (str/split (slurp input-file) #"\n")))