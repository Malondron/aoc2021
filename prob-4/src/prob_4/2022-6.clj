(ns src.prob-4.2022-6
  (:require [clojure.string :as str]))



(def input-file "d:\\programming\\aoc2021\\prob-4\\input-2022-6.txt")


(defn findStartOfMarker [chars lastfour pos]
  (if (= 0 (count chars))
    -1
    (if (and (> pos 3) (= 4 (count (set lastfour))))
      pos
      (recur (rest chars) (if (<= pos 3) (cons  (first chars) lastfour) (cons (first chars) (take 3 lastfour) )) (inc pos))
      )
    )
  
  )

(findStartOfMarker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" [] 0)
(findStartOfMarker (slurp input-file) [] 0)

(defn findStartOfMessage [chars lastfour pos]
  (if (= 0 (count chars))
    -1
    (if (and (> pos 13) (= 14 (count (set lastfour))))
      pos
      (recur (rest chars) (if (<= pos 13) (cons  (first chars) lastfour) (cons (first chars) (take 13 lastfour) )) (inc pos))
      )
    )
  
  )

(findStartOfMessage "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" [] 0)
(findStartOfMessage (slurp input-file) [] 0)