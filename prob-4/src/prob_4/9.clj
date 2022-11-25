(ns src.prob-4.9

  (:require [clojure.string :as str])
  )



(def input-file "d:\\programming\\aoc2021\\prob-4\\input-9.txt")

(str/split (slurp input-file) #"\n")
(def test-input ["2199943210" "3987894921" "9856789892" "8767896789" "9899965678"])

(defn parse-input [file]
  
 (str/split (slurp file) #"\n")
  
  )
 
(defn neibs [x y xmax ymax lines]
  (let [ln (nth lines y)
        fn (if (> x 0) (nth ln (dec x)))
        sn (if (< x xmax) (nth ln (inc x)))
        tn (if (> y 0) (nth (nth lines (dec y)) x))
        ftn (if (< y ymax) (nth (nth lines (inc y)) x))
        ]
    (filter some? [fn sn tn ftn])
    )
  )

(neibs 0 0 3 3 [[1 2 3 4][2 3 4 5][0 4 7 1][ 9 0 2 1]])

(defn test [x])

(defn findLows [lines]
  (let [rows (for [line lines] (map #(parse-long (str %)) line))
        ;res (test rows)
        xrange (count (first rows))
        yrange (count rows)]
      (for [x (range xrange)
            y (range yrange)
            
            :let [
                  val (nth (nth rows y) x) 
                  nbs (neibs x y (dec xrange) (dec yrange) rows)
                  lowpoints  (filter #(< % val) nbs)

                  ]
            :when (=  (count lowpoints) 0)
            ]
        [x y nbs lowpoints (+ 1 val)]
        )
    )
  )

(findLows test-input)
(findLows ( parse-input input-file))




