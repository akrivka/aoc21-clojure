(ns advent2021.day18
  (:require [clojure.string :as str]
            [clojure.zip :as z]
            [advent2021.util :refer [w-some]]))

(def main-input (slurp "src/advent2021/day18/day18.in"))

(def test-input (slurp "src/advent2021/day18/day18.example.in"))

(def test-input2 (slurp "src/advent2021/day18/day18.example2.in"))

(def example (read-string "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"))

(def example2 (read-string "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"))
;; -> [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map read-string)))

(def exzip (z/vector-zip example2))

(def exzip2 (z/vector-zip example))

(def exnode (-> exzip z/down z/down z/right z/down z/right z/down z/right))

(def exnode2 (-> exzip2 z/down z/down z/down z/down))

(def exnode3 (-> exzip z/down z/right z/down z/right z/down z/right z/down z/right z/down z/right))

(defn level [loc]
  (-> loc
      z/path
      count))

(def leaf? (complement z/branch?))

(defn add-left-old [loc n]
  (as-> loc l
    (w-some leaf? (rest (iterate z/prev l)))
    (z/edit l + n)
    (w-some leaf? (rest (iterate z/next l)))))

(defn add-left [loc n]
  (let [left-loc (some #(cond
                          (nil? %)  :nan
                          (leaf? %) %) (rest (iterate z/prev loc)))]
    (if (= left-loc :nan)
      loc
      (as-> left-loc l
        (z/edit l + n)
        (w-some leaf? (rest (iterate z/next l)))))))

(defn add-right-old [loc n]
  (as-> loc l
    (w-some leaf? (rest (iterate z/next l)))
    (z/edit l + n)
    (w-some leaf? (rest (iterate z/prev l)))))

(defn add-right [loc n]
  (let [right-loc (some #(cond
                           (z/end? %)  :nan
                           (leaf? %)   %) (rest (iterate z/next loc)))]
    (if (= right-loc :nan)
      loc
      (as-> right-loc l
        (z/edit l + n)
        (w-some leaf? (rest (iterate z/prev l)))))))

(defn explode-loc [loc]
  (let [[a b] (z/children loc)]
    (-> loc
        (z/replace 0)
        (add-left a)
        (add-right b))))

(defn explode-next
  [loc]
  (if (= :end (loc 1))
    loc
    (or
     (and (z/branch? loc) (every? number? (z/children loc)) (>= (level loc) 4)
          (explode-loc loc))
     (and (z/branch? loc) (z/down loc))
     (z/right loc)
     (loop [p loc]
       (if (z/up p)
         (or (z/right (z/up p)) (recur (z/up p)))
         [(z/node p) :end])))))

(defn explode [zipper]
  (z/vector-zip (z/node (w-some z/end? (iterate explode-next zipper)))))

(def exzip5 (z/vector-zip (read-string "[[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]")))

(defn split-next
  [loc]
  (if (= :end (loc 1))

    loc
    (or
     (and (leaf? loc) (>= (z/node loc) 10)
          (let [val      (z/node loc)
                half     (quot val 2)
                new-node (z/replace loc [half (- val half)])]
            [(z/root new-node) :end]))
     (and (z/branch? loc) (z/down loc))
     (z/right loc)
     (loop [p loc]
       (if (z/up p)
         (or (z/right (z/up p)) (recur (z/up p)))
         [(z/node p) :end])))))

(defn split [zipper]
  (z/vector-zip (z/node (w-some z/end? (iterate split-next zipper)))))

(defn sneduce [number]
  (loop [z (z/vector-zip number)]
    (let [nz (-> z explode split)]
      (if (= z nz)
        (first nz)
        (recur nz)))))

(defn find-magnitude [number]
  (or
   (and (vector? number) (+ (* 3 (find-magnitude (first number))) (* 2 (find-magnitude (second number)))))
   (and (number? number) number)
   "error!"))

(find-magnitude (read-string "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"))

(defn part1 [input]
  (let [[first-number & rest-numbers]  (parse-input input)
        result (reduce (fn [a b]
                         (sneduce [a b])) first-number rest-numbers)]
    (find-magnitude result)))

(defn part2 [input]
  (let [numbers (parse-input input)
        sums (for [a numbers
                   b numbers
                   :when (not= a b)]
               (-> [a b] sneduce find-magnitude))]
    (apply max sums)))

(def v [1 2 3])
(for [a v
      b v
      :when (not= a b)]
  [a b])

(comment
  (part2 main-input)
  ;;
  )

(println
 "Day 18\n"
 "- Part 1: " (part1 main-input)
 "- Part 2: " (part2 main-input))