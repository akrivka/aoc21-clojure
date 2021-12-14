(ns advent2021.day14
  (:require [clojure.string :as str]
            [advent2021.util :refer [+.]]))

(def main-input (slurp "src/advent2021/day14/day14.in"))

(def test-input (slurp "src/advent2021/day14/day14.test.in"))

(defn parse-input [input]
  (let [[template rules-raw] (str/split input #"\n\n")
        rules                (->> rules-raw
                                  (str/split-lines)
                                  (map #(let [[[g1 g2] [i]] (str/split % #" -> ")]
                                          [[g1 g2] i]))
                                  (into {}))]
    [template rules]))

(defn step [polymer rules]
  (let [pairs              (partition 2 1 polymer)
        [fpair :as ipairs] (map
                            (fn [[g1 g2 :as pair]]
                              [g1 (rules pair) g2])
                            pairs)]
    (apply str (first fpair) (flatten (map rest ipairs)))))

(defn part1 [input n]
  (let [[template rules] (parse-input input)
        result (nth (iterate #(step % rules) template) n)
        freq   (frequencies result)
        [_ most-common] (apply max-key second freq)
        [_ least-common] (apply min-key second freq)]
    (- most-common least-common)))

(defn step2 [freq rules]
  (let [updates (mapcat (fn [[[g1 g2 :as pair] n]]
                          (let [i (rules pair)]
                            [[[g1 g2] (- n)]
                             [[g1 i] n]
                             [[i g2] n]]))
                        freq)]
    (reduce (fn [fq [pair i]]
              (update fq pair +. i))
            freq
            updates)))

(defn part2 [input n]
  (let [[template rules] (parse-input input)
        initial-freq     (frequencies (partition 2 1 template))
        final-freq (nth (iterate #(step2 % rules) initial-freq) n)
        char-freq'  (reduce (fn [cfq [[g1 g2] n]]
                              (-> cfq
                                  (update g1 +. (/ n 2))
                                  (update g2 +. (/ n 2))))
                            {}
                            final-freq)
        char-freq (-> char-freq'
                      (update (first template) + 1/2)
                      (update (last template) + 1/2))
        [_ most-common] (apply max-key second char-freq)
        [_ least-common] (apply min-key second char-freq)]
    (- most-common least-common)))

(comment
  (part2 main-input 40)
  
  )



(println
 "Day 14\n"
 "- Part 1: " (part1 main-input 10)
 "- Part 2: " (part2 main-input 40)
 
 )