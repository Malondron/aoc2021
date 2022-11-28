(ns src.prob-4.10
  (:require [clojure.string :as str]))


(def start-chars "([{<")
(def end-chars ")]}>")

(def input-file "d:\\programming\\aoc2021\\prob-4\\input-10.txt")

(def test-input "{([(<{}[<>[]}>{[]{[(<()>")
(def test-input2 "[[<[([]))<([[{}[[()]]]")
(def test-input3 "[({(<(())[]>[[{[]{<()<>>")
(def large-test-input "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]")

(defn parse-input [inp]
  (str/split inp  #"\n")
  )

(parse-input large-test-input)
(.indexOf start-chars ")")

(defn getValue [inp-str]
  (case inp-str
    ")" 3
    "]" 57
    "}" 1197
    ">" 25137
    )
  )

(defn parseErr [inp-string start-chars end-chars starts]
  (if (= 0 (count inp-string))
    starts
    (let [sc (str (first inp-string))
          stind (.indexOf start-chars sc)
          endind (.indexOf end-chars sc)]
      (if (>= stind 0)
        (recur (subs inp-string 1) start-chars end-chars (conj starts sc))
        (let [endchar (str (nth start-chars endind))]
          (if (= endchar (last starts))
            (recur (subs inp-string 1) start-chars end-chars (vec (drop-last starts)))
            (recur "" start-chars end-chars (getValue (str (nth end-chars endind))))))))))

(defn solve1 [inp sc ec]
  (let [chunks (parse-input inp)]
    (reduce + (filter #(number? %) (map #(parseErr % sc ec []) chunks)))
    ))
  
  
  
(solve1 large-test-input start-chars end-chars)
(solve1 (slurp input-file) start-chars end-chars)
(map #(parseErr % start-chars end-chars []) (parse-input large-test-input))

