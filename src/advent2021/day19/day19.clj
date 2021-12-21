(ns advent2021.day19
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as com]
            [clojure.set :as set]
            [clojure.core.matrix :as mat]))


(def main-input (slurp "src/advent2021/day19/day19.in"))

(def test-input (slurp "src/advent2021/day19/day19.test.in"))

(defn parse-line [line]
  (as-> line l
    (str/split l #",")
    (map parse-long l)))

(defn det3d [[[a b c]
              [d e f]
              [g h i]]]
  (+ (* a e i)
     (* b f g)
     (* c d h)
     (* -1 c e g)
     (* -1 b d i)
     (* -1 a f h)))

(def all-rotation-matrices
  (mapcat identity
          (for [vs [[[1  0  0] [0  1  0] [0  0  1]]
                    [[-1 0  0] [0  1  0] [0  0  1]]
                    [[1  0  0] [0 -1  0] [0  0  1]]
                    [[1  0  0] [0  1  0] [0  0 -1]]
                    [[-1 0  0] [0 -1  0] [0  0  1]]
                    [[-1 0  0] [0  1  0] [0  0 -1]]
                    [[1  0  0] [0 -1  0] [0  0 -1]]
                    [[-1  0  0] [0 -1  0] [0  0 -1]]]]
            (filter #(= (det3d %) 1) (com/permutations vs)))))

(defn parse-input [input]
  (as-> input i
    (str/split i #"\n\n")
    (map (comp #(map parse-line %) rest str/split-lines) i)))

(defn max-overlap [s1 s2]
  (apply max-key first (for [ba s1
                             bb s2
                             :let [r       (map - ba bb)
                                   s2'     (map #(map + % r) s2)
                                   overlap (set/intersection (set s1) (set s2'))]]
                         [(count overlap) s2' r])))

(defn overlap-by-12? [s1 s2]
  (some
   (fn [s2']
     (let [[n s2'' r] (max-overlap s1 s2')]
       (when (>= n 12) [s2'' r])))
   (map (fn [r] (map #(mat/inner-product r %) s2)) all-rotation-matrices)))

(defn print-beacons [beacons]
  (->> beacons
       (sort-by first)
       (map #(str/join "," %))
       (str/join "\n")))

(defn find-beacons [scanners]
  (loop [{cb' :cb rs' :rs}    {:cb (set (first scanners))
                               :rs (rest scanners)}]
    (println "Scanners left: " (count rs'))
    (if (not-empty rs')
      (recur (reduce (fn [{:keys [cb rs]} s]
                       (if-let [[s'] (overlap-by-12? cb s)]
                         {:cb (into cb s')
                          :rs rs}
                         {:cb cb
                          :rs (conj rs s)})) {:cb cb'
                                              :rs []} rs'))
      cb')))

(defn part1 [input]
  (let [scanners (parse-input input)
        beacons  (find-beacons scanners)]
    (count beacons)))

(defn locate-scanners [scanners]
  (loop [{cb'   :cb
          rs'   :rs
          locs' :locs}    {:cb   (set (first scanners))
                           :rs   (rest scanners)
                           :locs #{[0 0 0]}}]
    (println "Scanners left: " (count rs'))
    (if (not-empty rs')
      (recur (reduce (fn [{:keys [cb rs locs]} s]
                       (if-let [[s' r] (overlap-by-12? cb s)]
                         {:cb (into cb s')
                          :rs rs
                          :locs (conj locs r)}
                         {:cb cb
                          :rs (conj rs s)
                          :locs locs})) {:cb cb'
                                         :rs []
                                         :locs locs'} rs'))
      locs')))

(defn part2 [input]
  (let [scanners       (parse-input input)
        scanners-loc   (locate-scanners scanners)
        manh-distances (for [s1 scanners-loc
                             s2 scanners-loc]
                         (reduce + (map - s1 s2)))]
    (apply max manh-distances)))

(comment
  (part1 main-input)
  (part2 main-input)
  ;;
  )

(println
 "Day 19\n"
 "- Part 1: " (part1 main-input)
 "- Part 2: " (part2 main-input))