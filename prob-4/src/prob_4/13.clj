(ns src.prob-4.13
  (:require [clojure.string :as str]))
 
(def input-file "d:\\programming\\aoc2021\\prob-4\\input-13.txt")

(def test-input "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5")

(str/split test-input #"\n\n")

(defn parse-input [inp]
  (let [m&i (str/split inp #"\n\n")
        ms  (mapv #(mapv parse-long (str/split % #",")) (str/split (first m&i) #"\n"))
        is (str/split (second m&i) #"\n")
        ]
    
    [ms is]
    ))

(def paper (first (parse-input test-input)))
(def instructions (second (parse-input test-input)))

(def paper-p1 (first (parse-input (slurp input-file))))
(def instructions-p1 (second (parse-input (slurp input-file))))

(defn printPap [pap maxx maxy]
  (doseq [
         
        y (range maxy)
        x (range maxx)
        :let
        [inpap? (> (count (filter #(= y (second %)) (filter #(= x (first %)) pap))) 0)
         fig (if inpap? "#" "*")]]
      (print fig)
      (when (= x (dec maxx))
        (print "\n"))
    ))

(printPap paper 11 15)

(defn getNewpaper [val xory paper]
  (set (for [pt paper
             :let [
                   x (first pt)
                   y (second pt)
                   isx? (= "x" xory)
                   newX (if isx? (if (> x val) (- val (- x val))  x) x)
                   newY (if isx? y (if (> y val) (- val (- y val))  y))
                   ]
             ]
         [newX newY]
         
         ))
  )

(defn dotoPaper [paper inst xmax ymax]
  (let [wandval (str/split inst #"=")
        val (parse-long (last wandval))
        xory  (str (last (first wandval)))
        newxmax (if (= xory "x") val xmax)
        newymax (if (= xory "y") val ymax)
        newpaper (getNewpaper val xory paper)
        ]
    [newpaper newxmax newymax]
    )
  )

(defn allInstrs [instrs paper xmax ymax]
  (if (= 0 (count instrs))
    [paper xmax ymax]
    (let [newPaper (dotoPaper paper (first instrs) xmax ymax)]
      (recur (rest instrs) (first newPaper) (second newPaper) (nth newPaper 2))
      )
    )
  )

(count (first (dotoPaper paper (first instructions) 11 15)))
(apply max (map second paper-p1))
(count (first (dotoPaper paper-p1 (first instructions-p1) 1310 894)))
(let [finals (allInstrs instructions-p1 paper-p1 1310 894)]
  (printPap (first finals) (second finals) (nth finals 2))
  )