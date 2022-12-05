(ns src.prob-4.2022-5
  (:require [clojure.string :as str]))



(def input-file "d:\\programming\\aoc2021\\prob-4\\input-2022-5.txt")

(def test-input "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(defn parse-input [inp]
  (let [lines (str/split inp #"\n")
        starts (take-while #(.contains % "[") lines)
        stackpos (count starts)
        stacks (nth lines stackpos)
        movepos (+ 2 stackpos)
        ]
    [starts stacks (drop movepos lines)]
    ))

(defn buildStacks [stacks out]
  (if (= 0 (count stacks))
    (let [outc (count (first out))]
      (for [i (range outc)]
        (filter #(not (= " " %)) (mapv #(nth % i)  out)) 
        )
      )
    (recur (rest stacks) (cons (mapv str (mapv second (partition 4 (str (first stacks) " ")))) out)) 

    )
  )

(defn buildStart [inp]
  (let [nr-stacks (count (str/split (second inp) #"\d "))
        starts (first inp)
        ]

   (buildStacks starts []) 
    )
  )

(mapv str (mapv second (partition 4 (str (first (first (parse-input test-input))) " "))))
(def start (buildStart (parse-input test-input)))

(defn makeMove [move stacks]
  (let [movep (str/split move #" ")
        nrblocks (parse-long (nth movep 1))
        from (parse-long (nth movep 3))
        to (parse-long (nth movep 5))
        fromv (nth stacks (dec from))
        frompos (count (filter #(not (= " " %)) fromv))
        toadd (reverse (filter #(not (= " " %)) (drop (- frompos nrblocks) fromv )))
        tov (nth stacks (dec to))
        topos (count (filter #(not (= " " %)) tov))
        ]
    
    (println (reduce + (map count  stacks)))
    (for [s (range (count stacks))]
      (if (= s (dec to))
        (concat (take topos tov) toadd)
        (if (= s (dec from)) 
          (take (- frompos nrblocks) fromv)
          (nth stacks s)
          )
        )
      )
    ) 
  )
(makeMove "move 1 from 2 to 1"  start)

(defn makeMoves [moves stacks]
  (if (= 0 (count moves))
    stacks
    (recur (rest moves) (makeMove (first moves) stacks))
    )
  )

(makeMoves (nth (parse-input test-input) 2) start)
(defn solv1 [inp]
  (let [start (buildStart inp)]
    (apply str (map last (makeMoves (nth inp 2) start)))
    
    )
  )

(solv1 (parse-input test-input))
(solv1 (parse-input (slurp input-file)))