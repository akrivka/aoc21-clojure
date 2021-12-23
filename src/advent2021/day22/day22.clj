(ns advent2021.day22
  (:require [clojure.string :as str]
            [clojure.core.reducers :as r]
            [clojure.math.combinatorics :as com]
            [clj-async-profiler.core :as prof]))

(def main-input (slurp "src/advent2021/day22/day22.in"))

(def main-input-part1 (slurp "src/advent2021/day22/day22-part1.in"))

(def test-input (slurp "src/advent2021/day22/day22.test.in"))

(def test-input2 (slurp "src/advent2021/day22/day22.test2.in"))

(def test-input3 (slurp "src/advent2021/day22/day22.test3.in"))


(def micro-input "on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10")

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map #(let [[[_ state' & coords']] (re-seq #"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)" %)
                   state                  (case state' "on" 1 "off" 0)
                   coords                 (partition 2 (map parse-long coords'))]
               [state coords]))))

(re-seq #"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)" "on x=-20..26,y=-36..17,z=-47..7")

(defn is-within-box? [box point]
  (assert (= (count box) (count point)))
  (every? (fn [[[x1 x2] x]]
            (and (>= x x1) (<= x x2))) (map vector box point)))

(defn get-final-state [instructions point]
  (some
   (fn [[state box]]
     (when (is-within-box? box point)
       state))
   instructions))

(defn part1 [input]
  (let [instructions (parse-input input)
        final-states (reduce #(+ %1 (or (get-final-state instructions %2) 0))
                             0
                             (for [x (range -50 51)
                                   y (range -50 51)
                                   z (range -50 51)]
                               [x y z]))]
    final-states))

(defn part1-concurrent [input]
  (let [instructions' (parse-input input)
        instructions (reverse instructions')
        final-states (r/fold
                      +
                      #(+ %1 (or (get-final-state instructions %2) 0))
                      (into [] (for [x (range -50 51)
                                     y (range -50 51)
                                     z (range -50 51)]
                                 [x y z])))]
    final-states))



(defn part2 [input]
  (let [instructions' (parse-input input)
        instructions (reverse instructions')
        all-points (into #{} (for [x (range -50 51)
                                   y (range -50 51)
                                   z (range -50 51)]
                               [x y z]))
        {:keys [on]} (loop [on 0
                            points all-points
                            [[state [xb yb zb] :as ins] & rinstructions] instructions]
                       (println ins)
                       (if ins
                         (if (not-empty points)
                           (let [next-points (into #{} (for [x (apply range xb)
                                                             y (apply range yb)
                                                             z (apply range zb)]
                                                         [x y z]))]
                             (recur (if (= state 1) (count next-points) on)
                                    (apply disj points next-points)
                                    rinstructions))
                           on)
                         on))]
    on))

(defn part2' [input]
  (let [instructions' (parse-input input)
        instructions  (reverse instructions')
        on            (reduce
                       (fn [on [state [xb yb zb]]]
                         (let [points (into #{} (for [x (apply range xb)
                                                      y (apply range yb)
                                                      z (apply range zb)]
                                                  [x y z]))]
                           (case state
                             0 (apply disj on points)
                             1 (apply conj on points))))
                       #{}
                       instructions)]
    (count on)))

(com/cartesian-product [1 2 3] [:a :b :c])

(defn intersect-line [[xa1 xa2] [xb1 xb2]]
  (cond
    ;; a contained in b
    (and (>= xa1 xb1) (<= xa2 xb2)) [[]
                                     [[xa1 xa2]]
                                     [[xb1 (dec xa1)] [(inc xa2) xb2]]]
    ;; b contained in a
    (and (>= xb1 xa1) (<= xb2 xa2)) [[[xa1 (dec xb1)] [(inc xb2) xa2]]
                                     [[xb1 xb2]]
                                     []]
    ;; a right overlaps b
    (and (>= xa1 xb1) (<= xa1 xb2) (> xa2 xb2)) [[[(inc xb2) xb2]]
                                                 [[xa1 xb2]]
                                                 [[xb1 (dec xa1)]]]
    ;; b right overlaps a
    (and (>= xb1 xa1) (<= xb1 xa2) (> xb2 xa2)) [[[xa1 (dec xb1)]]
                                                 [[xb1 xa2]]
                                                 [[(inc xa2) xb2]]]))

(defn intersect-box [box1 box2]
  (let [intersections (map intersect-line box1 box2)]
    (when (every? (comp not nil?) intersections)
      (let [[[_ & first-diff]
             [_ & second-diff]] (->> intersections
                                     (map (fn [[ab i ba]]
                                            [(concat i ab)
                                             (concat i ba)]))
                                     (apply map com/cartesian-product))
            intersection (map (comp first second) intersections)]
        [first-diff intersection second-diff]))))

(def box1 [[0 5] [0 5]])

(def box2 [[1 2] [3 4]])

(intersect-box box1 box2)


(defn difference-line [[a1 a2] [b1 b2]]
  (cond
    ;; b in a
    (and (> b1 a1) (< b2 a2)) [[a1 (dec b1)] [(inc b2) a2]]
    ;; a in b
    (and (>= a1 b1) (<= a2 b2)) []
    ;; a right overlaps b
    (and (> a1 b1) (<= a1 b2)
         (>= a2 b2)) [[(inc b2) a2]]
    ;; b right overlaps a
    (and (> b1 a1) (<= b1 a2)
         (>= b2 a2)) [[a1 (dec b1)]]
    ;; no intersect
    :else [[a1 a2]]))

(defn intersection-line [[a1 a2] [b1 b2]]
  (cond
    ;; b in a
    (and (> b1 a1) (< b2 a2)) [b1 b2]
    ;; a in b
    (and (>= a1 b1) (<= a2 b2)) [a1 a2]
    ;; a right overlaps b
    (and (> a1 b1) (<= a1 b2)
         (>= a2 b2)) [a1 b2]
    ;; b right overlaps a
    (and (> b1 a1) (<= b1 a2)
         (>= b2 a2)) [b1 a2]
    ;; no intersect
    :else nil))

(def line1 [0 5])
(def line2 [-1 3])

(intersection-line line1 line2)

(defn difference-box [box1 box2]
  (let [differences   (map difference-line   box1 box2)
        intersections (map intersection-line box1 box2)]
    (if (every? identity intersections)
      (rest
       (apply com/cartesian-product (map #(concat [%1] %2) intersections differences)))
      [box1])))

(def box3 [[0 5] [0 5]])
(def box4 [[6 6] [6 6]])

(difference-box box3 box4)

(defn string-boxes [boxes]
  (->> boxes
       (into #{}
        (mapcat (fn [[[x1 x2] [y1 y2] [z1 z2]]]
                  (for [x (range x1 (inc x2))
                        y (range y1 (inc y2))
                        z (range z1 (inc z2))]
                    [x y z]))))
       (sort-by first)
       (map #(str/join "," %))
       (str/join "\n")))

(defn size [box]
  (transduce (map (fn [[x1 x2]] (inc (- x2 x1)))) * box))

(defn merge-in-instruction [on-boxes [state box]]
  (into (case state 0 #{} 1 #{box})
        (mapcat #(difference-box % box))
        on-boxes))

(defn part3 [input]
  (let [instructions (parse-input input)
        on-boxes      (reduce
                       merge-in-instruction
                       #{}
                       instructions)]
    (transduce (map size) + on-boxes)))

(comment
  (prof/serve-files 8080)

  (- 601212 601104)
  (- (part3 main-input) 108)
  ;;
  )

(println
 "Day 22\n"
 "- Part 1: " (part1 main-input)
 "- Part 2: " (part2 main-input))