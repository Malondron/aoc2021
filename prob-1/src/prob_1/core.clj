(ns prob-1.core
(:require [clojure.string :as str])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(def input-file "C:\\programming\\aoc2021\\prob-1\\input.txt")

(defn recDown [vals prev-val cnt]
  (if (= 0 (count vals))
    cnt
    (recur (rest vals) (first vals) (if (> (first vals) prev-val) (inc cnt) cnt))
    ))

(recDown [199 200 208 210 200 207 240 269 260 263] 10000 0)

  (defn solve1 [file]
    (let [invals (map #(Integer/parseInt %) (str/split (slurp file) #"\n"))]
      (recDown invals 10000 0)))

(solve1 input-file)

(defn recDown2 [vals prev-val cnt]
  (if (= 2 (count vals))
    cnt
    (let [th-sum (reduce + (subvec vals 0 3))]
      (recur (vec (rest vals)) th-sum (if (> th-sum prev-val) (inc cnt) cnt)))))

(recDown2 [199 200 208 210 200 207 240 269 260 263] 10000 0)
(reduce + [199 200 208 210 200 207 240 269 260 263])

(defn solve2 [file]
    (let [invals (vec (map #(Integer/parseInt %) (str/split (slurp file) #"\n")))]
      (recDown2 invals 10000 0)))

(solve2 input-file)

  
(str/split (slurp input-file) #"\n")

