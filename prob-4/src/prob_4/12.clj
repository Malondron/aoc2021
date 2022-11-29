(ns src.prob-4.12
  (:require [clojure.string :as str]))



(def input-file "d:\\programming\\aoc2021\\prob-4\\input-12.txt")
(def test-input
  "start-A
start-b
A-c
A-b
b-d
A-end
b-end"
  )
(def test-input-2
"dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc"
  
  )

(def test-input-3
  "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW"
  
  )


(apply hash-map ["hj"  [ "j" "k"]])

(defn findInPairs [el pairs]
  (remove nil? (for [p pairs]
                 (if (= el (first p))
                   (second p)
                   (if (= el (second p))
                     (first p)) )
                 )))

(defn parse-input [inp]
  (let [pairs (map #(str/split % #"-") (str/split inp #"\n"))
        uniq (set (flatten pairs))
        mapish (for [el uniq]


                 [el (findInPairs el pairs)])]
  (zipmap (map first mapish) (map second mapish))
    )
  )

(parse-input test-input)

(def pathmap (parse-input (slurp input-file)))
(def pathmap (parse-input test-input))
(def pathmap (parse-input test-input-2))
(def pathmap (parse-input test-input-3))

(defn notAllowed [el visited]
  (and (Character/isLowerCase (first el)) (.contains visited el ))
  )

(defn walkFromTo [pthmp to visited corr-paths]
  (if (= to (last visited))
    (conj corr-paths visited)
    (let [conn-moves (get pthmp (last visited))
          poss-moves (filter #(not (notAllowed % visited)) conn-moves)]
      
      (concat (mapcat #(walkFromTo pthmp to (conj visited %) corr-paths) poss-moves
                      


                      ))
      ))
  )

(count (walkFromTo pathmap "end" ["start"] []))