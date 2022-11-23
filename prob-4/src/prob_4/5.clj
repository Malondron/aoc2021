(ns src.prob-4.5
  
  (:require [clojure.string :as str])
  
  )


(def input-file "d:\\programming\\aoc2021\\prob-4\\input-5.txt")

(slurp input-file)


(defn isHorVert? [x y]
  (or (= (nth x 0) (nth y 0)) (= (nth x 1) (nth y 1))))

(defn new-points [line]
  (let [fp (first line)
        sp (second line)
        x0 (first fp)
        y0 (second fp)
        x1 (first sp)
        y1 (second sp)]

    (for [i (range (min x0 x1) (+ 1 (max x0 x1)))
          j (range (min y0 y1) (+ 1 (max y0 y1)))]

        [i j])))

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
  (let [inList (str/split (slurp file) #"\n")
        lines (map parseLineLine inList)
        ]

    (->> (collPoints lines  [])
         flatten
         (partition 2)
         frequencies
         vals
         (filter #(> % 1))
         count)))

(solve1 input-file)
(parseLineLine (first (str/split (slurp input-file) #"\n")))

(defn isHorVertDiag? [x y]
  (or (= (nth x 0) (nth y 0)) (= (nth x 1) (nth y 1))
      (= 1 (/ (abs (- (nth x 1) (nth y 1)))  (abs (- (nth x 0) (nth y 0))) ) ))
  
  )

(defn new-points-2 [line]
  (let [fp (first line)
        sp (second line)
        x0 (first fp)
        x1 (first sp)
        minxp (if (< x0 x1) fp sp)
        maxxp (if (< x0 x1) sp fp)
        slope (/ (- (nth maxxp 1) (nth minxp 1)) (- (nth maxxp 0) (nth minxp 0)))]
    (for [i (range (+ 1 (abs (- x0 x1))))]

      [(+ (nth minxp 0) i) (+ (nth minxp 1) (* slope  i ))])
  )) 

(new-points-2 [[0 9] [9 0]])))
(pointsFromLine [[5 5] [8 2]])))
(def test-lines "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2")

(defn pointsFromLine [line]
  (if (isHorVert? (first line) (second line))
    (new-points line)
    (if (isHorVertDiag? (first line) (second line))
      (new-points-2 line)
      
      
      ))
  )

(defn collPoints-2 [in-lines outlist]
  (if (= 0 (count in-lines))
    outlist
    (let [new-points (new-points-2 (first in-lines))]
      
      (recur (rest in-lines) (apply conj outlist new-points)))
    )
  )







(defn solve2 [file]
  (let [inList (str/split (slurp file) #"\n")
        lines (map parseLineLine inList)]
    (->> 
        (mapcat pointsFromLine lines)
        ; flatten
        ; (partition 2)
         frequencies
         vals 
         (filter #(> % 1))
         )))
    ;frequencies
    ;     vals
    ;     (filter #(> % 1))
     ;    count)
   ; )


(count (solve2 input-file))
(count)(solve2 test-lines)
(isHorVertDiag? [8 0] [0 7] )