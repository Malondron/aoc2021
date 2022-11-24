(ns src.prob-4.8

  (:require [clojure.string :as str])
  )


(def input-file "d:\\programming\\aoc2021\\prob-4\\input-8.txt")

(str/split (str/trim (second (str/split (first (str/split (slurp input-file) #"\n")) #"\|") )) #" ") 

(defn parse-input [file]
  (mapv #(str/split (str/trim (second (str/split % #"\|"))) #" ") (str/split (slurp file) #"\n"))
  )
(parse-input input-file)
(def test-string "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf" )



(defn countLine [line]
  (let [lens (map count line)
        founds (filter #(.contains [2 4 3 7] %) lens)]
    (count founds )))


(countLine ["fdgacbe" "cefdb" "cefbgd" "gcbe"])

(defn count1478 [inlines cnt]
  (if (= 0 (count inlines))
    cnt
    (let [lncnt (countLine (first inlines))]
      
      (recur (rest inlines) (+ lncnt cnt))
      )
    )
  )

(count1478 (parse-input input-file) 0)
(range 10)

(defn analyzeSeven [seven one]
  (str (first (filter #(not (str/includes? one (str %))) seven)))
  )

(analyzeSeven "dab" "ab")

(defn analyzeFour [four one]
  (map str (filter #(not (str/includes? one (str %))) four))
  )

(analyzeFour "eafb" "ab")

(defn analyzeZero [zerosixnine fourres]
 (first (for [v zerosixnine
              :let [fr (str/includes? v (first fourres))
                    sr (str/includes? v (second fourres))]
              :when (not (and fr sr))
              ]
          [v (if fr [(second fourres) (first fourres)] [ (first fourres) (second fourres)])]
          ))
)

(analyzeZero ["cefabd" "cdfgeb" "cagedb"] ["e" "f"])

(defn analyzeSixandNines [zerosixnine zero one]
  (let [sixnine (filter #(not (= zero %)) zerosixnine)
        six (first (filter #(not (and (str/includes? % (str (first one))) (str/includes? % (str (second one))))) sixnine))
        nine (first (filter #(not (= six %)) sixnine))
        ]
    [ [six (if (str/includes? six (str (first one))) [(str (second one)) (str (first one))] [(str (first one)) (str (second one))]    )] [nine]]
 
    )
  )

(defn findLast [nine eight]
  (str (first (filter #(not (str/includes? nine (str %))) eight)))
  )

(findLast "cefabd" "acedgfb")


(defn analyzezerosixnine [zerosixnine one fourres eight]
  (let [zerores (analyzeZero zerosixnine fourres)
        sixnadnineres (analyzeSixandNines zerosixnine (first zerores) one)
        findlast (findLast (first (second sixnadnineres)) eight)
        ]
    [zerores sixnadnineres findlast]
    )
  )


(defn findSev [map]
   (let [found (reduce str (vals @map))
         tot "abcdefg"

         ]
    (str (first (filter #(not (str/includes? found (str %))) tot)))
     ) 
  )


(defn findSpec [ttf props symbs]
  (first (filter #(and (str/includes? % (get @symbs (first props)))  (str/includes? % (get @symbs (second props)))  ) ttf))
  )

(defn get235 [ttf symbs]
  (let [two (findSpec ttf [:three :five] symbs)
        three (findSpec ttf [:three :six] symbs)
        five (findSpec ttf [:two :six] symbs)]
    [two three five]
    )
  )

(defn analyzeLine [line]
  (def outsymbs (atom {}))
  (def outnumbs (atom {}))
  (let [one (first (filter #(= 2 (count %)) line))
        seven (first (filter #(= 3 (count %)) line))
        four (first (filter #(= 4 (count %)) line))
        zerosixnine (filter #(= 6 (count %)) line)
        twothreefive (filter #(= 5 (count %)) line)
        eight (first (filter #(= 7 (count %)) line))
        sevres (analyzeSeven seven one)
        fourres (analyzeFour four one)
        zsnres (analyzezerosixnine zerosixnine one fourres eight)]

    (swap! outnumbs assoc :one one)
    (swap! outnumbs assoc :zero (first (first zsnres)))
    (swap! outnumbs assoc :six (first (first (second zsnres))))
    (swap! outnumbs assoc :nine (first (second (second zsnres))))
    (swap! outnumbs assoc :seven seven)
    (swap! outnumbs assoc :four four)
    (swap! outnumbs assoc :eight eight)
    (swap! outsymbs assoc :one sevres)
    (swap! outsymbs assoc :four (first (second (first zsnres))))
    (swap! outsymbs assoc :five (nth zsnres 2))
    (swap! outsymbs assoc :two (second (second (first zsnres))))
    (swap! outsymbs assoc :three (first (second (first (second zsnres)))))
    (swap! outsymbs assoc :six (second (second (first (second zsnres)))))
    (let [seventh (findSev outsymbs)]

      (swap! outsymbs assoc :seven seventh))
   (let [ttf (get235 twothreefive outsymbs)]
     

    (swap! outnumbs assoc :two (first ttf))
    (swap! outnumbs assoc :three (second ttf))
    (swap! outnumbs assoc :five (nth ttf 2))
      @outnumbs)

  ))

(analyzeLine [ "acedgfb" "cdfbe" "gcdfa" "fbcad" "dab" "cefabd" "cdfgeb" "eafb" "cagedb" "ab"])
