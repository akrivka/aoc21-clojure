(ns advent2021.day4
  (:require [clojure.string :as str]))

(def input
  (slurp "src/advent2021/day4/day4.in"))

(def test-input
  (slurp "src/advent2021/day4/day4.test.in"))

(def test-board
  [[[1 true]  [2 true]  [3 true]]
   [[4 true] [5 false] [6 false]]])


(defn parse-input [input]
  (let [[-draws & -boards] (str/split input #"\n\n")
        draws (map parse-long (str/split -draws #","))
        boards (map 
                (fn [board] 
                  (as-> board b
                    (str/split b #"\n")
                    (map #(str/split (str/trim %) #"\s+") b)
                    (map #(map parse-long %) b))) -boards)]
    [draws boards]))

(defn pad-string-left [s n]

  (let [w (- n (count s))
        p (apply str (repeat w " "))]
    (str p s)))

(defn prepare-board [board]
  (map #(map (fn [x] [x false]) %) board))

(defn mark-off-number [board n]
  (map #(map (fn [[x m]] [x (or m (= x n))]) %) board))

(mark-off-number test-board 5)

(defn winning-row? [board]
  (some #(every? (fn [[x m]] m) %) board))

(defn winning-board? [board]
  (or (winning-row? board)
      (winning-row? (apply map vector board))))

(defn is-there-winner? [boards]
  (some #(when (winning-board? %) %) boards))

(defn make-bold [s]
  (str "\033[1m" s "\033[0m"))

(defn stringify-board [board]
  (->> board
    (map #(map (fn [[x m]] 
                (let [px (pad-string-left (str x) 2)]
                  (if-not m px (make-bold px)))) %))
    (map #(str/join " " %))
    (str/join "\n")))

(defn print-board [board]
  (println (stringify-board board))
  (println "\n"))

(defn get-winning-board [draws boards]
  (let [prepared-boards (map prepare-board boards)]
    (loop [[n & r] draws bs prepared-boards]
      (let [next-boards (map #(mark-off-number % n) bs)]
        (if (empty? r)
          "no winner"
          (if-let [winner (is-there-winner? next-boards)]
            [winner n]
            (recur r next-boards)))))))

(defn get-sum-of-unmarked-board [board]
  (let [unmarked (reduce #(into %1 (keep (fn [[x m]] (when-not m x)) %2)) [] board)]
    (apply + unmarked)))

(defn part1
  ([] (part1 input))
  ([input]
    (let [[draws boards]                  (parse-input input)
          [winning-board winning-number]  (get-winning-board draws boards)
          unmarked-sum                    (get-sum-of-unmarked-board winning-board)]
      (* unmarked-sum winning-number))))

(defn split-winners-and-losers [next-boards]
  [(keep #(when (winning-board? %) %) next-boards)
   (keep #(when-not (winning-board? %) %) next-boards)])

(defn get-last-winning-board [draws boards]
  (let [prepared-boards (map prepare-board boards)]
    (loop [[n & r] draws bs prepared-boards lwbp nil]
      (let [next-boards     (map #(mark-off-number % n) bs)
            [[winner] losers] (split-winners-and-losers next-boards)]
        (if (empty? r)
          lwbp
          (recur r losers (if winner [winner n] lwbp)))))))

(defn part2
  ([] (part2 input))
  ([input]
    (let [[draws boards]                  (parse-input input)
          [winning-board winning-number]  (get-last-winning-board draws boards)
          unmarked-sum                    (get-sum-of-unmarked-board winning-board)]
      (* unmarked-sum winning-number))))

(comment
  (part1)
  (part2)
  )
