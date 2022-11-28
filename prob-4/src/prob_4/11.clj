(ns src.prob-4.11
  (:require [clojure.string :as str]))

(defn test [x] x)


(defn parse-input [inp]
  (let [lines (str/split inp #"\n")
        x-len (count (map #(parse-long (str %)) (first lines)))]
    
    (apply hash-map (flatten (for [y (range (count lines))
                                   x (range x-len)
                                   :let [
                                         
                                         ln (nth lines y)
                                         ln-nmbs (map #( parse-long (str %)) ln)
                                         ]
                                   ]
                               [ (str x y) (nth ln-nmbs x)]))) 
      ))

(def input-file "d:\\programming\\aoc2021\\prob-4\\input-11.txt")
(slurp input-file)
(def test-input "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")

(get (parse-input test-input) "56")
(str/split test-input #"\n")


(def octGrid (atom (parse-input (slurp input-file))))
(get @octGrid "56")
(swap! octGrid update-in ["56"] inc)


(defn neighbors [x y maxx maxy]
  (for [
         nx (range (max 0 (dec x)) (inc (min (inc x) maxx)))
         ny (range (max 0 (dec y)) (inc (min (inc y) maxy)))
        :when (not (and (= x nx) (= y ny))) 
        ]
    [nx ny]
    )
  )

(neighbors 8 2 8 9)

(defn makeOctStep [octs pos maxx maxy]
 ; (println pos)
  (let [x (first pos)
        y (second pos)
        val (get @octs (str x y))]
    
    (if (< -1 val 9)
      (swap! octs update-in [(str x y)] inc)
      (when (= val 9)
        (let [neibs (neighbors x y maxx maxy)]
#_{:clj-kondo/ignore [:invalid-arity]}
 ;(println "bam!")
;  (println [x y])
 ; (println neibs)
 (swap! octs assoc (str x y) -1)
 (doall (map #(makeOctStep octs % maxx maxy) neibs)))
          
          )))
  )

(makeOctStep octGrid [1 7] 9 9)

(defn solv1 [octGrid xmax ymax steps step flashes]
  (if (= steps step)
    flashes
    (do
      (doseq [x (range (inc xmax))
              y (range (inc ymax))]
        (makeOctStep octGrid [x y] xmax ymax))
      (doseq [x (range (inc xmax))
              y (range (inc ymax))]
        (when (= -1 (get @octGrid (str x y)))
           (swap! flashes inc)
          (swap! octGrid assoc (str x y) 0)))
    (recur octGrid xmax ymax steps (inc step) flashes)
      )))

(def count (atom 0))

(solv1 octGrid 9 9 100 0 count)
(printGrid octGrid 9 9)
(get @octGrid "00")
(get @octGrid "00")

(defn printGrid [grid xmax ymax]
  (doseq [
        y (range (inc ymax))
        x (range (inc xmax))
        ]
      (print (get @grid (str x y)))
      (when (= x xmax)
        (print "\n"))
  )
  )

(printGrid octGrid 9 9)