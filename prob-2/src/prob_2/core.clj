(ns prob-2.core
(:require [clojure.string :as str])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(def input-file "C:\\programming\\aoc2021\\prob-2\\input.txt")

(defn recPos [valstr pos]
  (if (= 0 (count valstr))
    pos
    (let [vals (str/split (first valstr) #" ")
          newval (Integer/parseInt (second  vals))
          newpos (case (first  vals)
                   "forward" [(+ newval (first pos)), (second pos)]
                   "down" [(first pos), (+ (second pos) newval)]
                   "up" [(first pos), (- (second pos) newval)]
                   )]
      (recur (rest valstr) newpos))))

(recPos ["forward 5" "down 5" "forward 8" "up 3" "down 8" "forward 2"] [0 0])

  (defn solve1 [file]
    (let [invals (str/split (slurp file) #"\n")
          ret (recPos invals [0 0])]
      (* (first ret) (second ret))
      ))

(defn recPos2 [valstr pos]
  (if (= 0 (count valstr))
    pos
    (let [vals (str/split (first valstr) #" ")
          newval (Integer/parseInt (second vals))
          newpos (case (first  vals)
                   "forward" [(+ newval (first pos)), (+ (second pos) (* newval (nth  pos 2))) (nth pos 2)]
                   "down" [(first pos), (second pos) (+ (nth pos 2) newval)]
                   "up" [(first pos), (second pos) (- (nth pos 2) newval)]
                   )]
      (recur (rest valstr) newpos))))

(recPos2 ["forward 5" "down 5" "forward 8" "up 3" "down 8" "forward 2"] [0 0 0])
(solve1 input-file)
 (defn solve2 [file]
    (let [invals (str/split (slurp file) #"\n")
          ret (recPos2 invals [0 0 0])]
      (* (first ret) (second ret))
      ))


(solve2 input-file)

  
(str/split (slurp input-file) #"\n")

