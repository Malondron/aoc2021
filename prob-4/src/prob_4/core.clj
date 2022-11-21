(ns prob-4.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(def input-file "C:\\programming\\aoc2021\\prob-4\\input.txt")

(defn findMaxPostion [numbers positions]
  (apply max (for [numb numbers]
               (.indexOf positions numb))))


(defn findMinOfBoard [board positions]
  (let [maxRows (apply min (for [row board]
                             (findMaxPostion row positions)))
        maxCols (apply min (for [col (range (count (first board)))]

                             (findMaxPostion (map #(nth % col) board) positions)))]

    (min maxRows maxCols)))

(findMaxPostion [14 21 17 24 4] [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1])
(findMinOfBoard [[22 13 17 11  0][ 8  2 23  4 24][21  9 14 16  7][ 6 10  3 18  5][ 1 12 20 15 19]] [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1])


(defn findBestBoard [boards positions]
  (for [board boards]
    [board (findMinOfBoard board positions)]
    )
  )

(findBestBoard [ [[22 13 17 11  0] [ 8  2 23  4 24] [21  9 14 16  7] [ 6 10  3 18  5] [ 1 12 20 15 19]]
                [[3 15  0  2 22][9 18 13 17  5][19  8  7 25 23][20 11 10 24  4][14 21 16 12  6]]
                [[14 21 17 24  4][10 16 15  9 19][18  8 23 26 20][22 11 13  6  5][2  0 12  3  7]]
                ] [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1])


(defn scoreBoard [board lastpos positions]
  (def sum (atom 0))
  (println board)
     (doseq [row board]
       (doseq [col (range (count row))]
         (let [val (nth row col)
               fpos (.indexOf positions val)]
           (when (< lastpos fpos)
              (swap! sum #(+ % val))))))
    ; (print "hkh")
  ;(print @sum)
  (* (nth positions lastpos) @sum))

(scoreBoard [[14 21 17 24  4] [10 16 15  9 19] [18  8 23 26 20] [22 11 13  6  5] [2  0 12  3  7]] 11 [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1] )


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
        oxy (findOxy invals)
        co2 (findCO2 invals)
        ]
    (* (Long/parseLong (reduce str (map first ret)) 2)
       (Long/parseLong (reduce str (map second ret)) 2))))



(solve2 input-file)


(str/split (slurp input-file) #"\n")

