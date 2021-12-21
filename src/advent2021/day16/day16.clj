(ns advent2021.day16
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :refer [expt]]))

(def main-input (slurp "src/advent2021/day16/day16.in"))

(def hex->binary-digits
  {\0 [0 0 0 0]
   \1 [0 0 0 1]
   \2 [0 0 1 0]
   \3 [0 0 1 1]
   \4 [0 1 0 0]
   \5 [0 1 0 1]
   \6 [0 1 1 0]
   \7 [0 1 1 1]
   \8 [1 0 0 0]
   \9 [1 0 0 1]
   \A [1 0 1 0]
   \B [1 0 1 1]
   \C [1 1 0 0]
   \D [1 1 0 1]
   \E [1 1 1 0]
   \F [1 1 1 1]})

(defn parse-input [input]
  (->> input
       (mapcat hex->binary-digits)))

(defn binary->decimal [bs]
  (second
   (reduce
    (fn [[e r] b]
      [(inc e) (if (= b 1)
                 (+ r (expt 2 e))
                 r)])
    [0 0]
    (reverse bs))))

(defn type-id->type [type-id]
  (case type-id
    4 :literal
    nil :end
    :operator))

(defn parse [binary]
  (loop [r binary
         t :header
         p []]
    (case t
      :header   (let [[version type-id] (->> (partition 3 r)
                                             (take 2)
                                             (map binary->decimal))]
                  (when-let [type (type-id->type type-id)]
                    (if (or (= type :end) (every? #(= % 0) r))
                      p
                      (recur (drop 6 r) type (conj p {:version version :type-id type-id})))))
      :literal  (let [[digits' [last-digit' & nr']] (split-with #(= (first %) 1) (partition 5 5 (repeat 5 nil) r))
                      digits                        (->> (concat digits' [last-digit'])
                                                         (map rest)
                                                         (flatten))
                      nr                             (remove nil? (flatten nr'))]
                  [nr (update p (dec (count p)) assoc :value (binary->decimal digits)) (+ 6 (- (count r) (count nr)))])
      :operator (let [[length-type-id & content'] r]
                  (case length-type-id
                    0 (let [[total-length' content] (split-at 15 content')
                            total-length (binary->decimal total-length')]
                        (loop [c  content
                               pp p
                               tl 0]
                          (if (< tl total-length)
                            (let [[nr npp dtl] (parse c)]
                              (recur nr (update pp 1 conj npp) (+ tl dtl)))
                            [c pp (+ 6 (- (count r) (count c)))])))

                    1 (let [[total-number' content] (split-at 11 content')
                            total-number (binary->decimal total-number')]
                        (loop [c  content
                               pp p
                               tl 0]
                          (if (not= (count (second pp)) total-number)
                            (let [[nr npp dtl] (parse c)]
                              (recur nr (update pp 1 conj npp) (+ tl dtl)))
                            [c pp (+ 6 (- (count r) (count c)))])))))
      "error")))

(defn dummy [] ())

(defn parse-all [binary]
  (loop [r binary
         p []]
    (if (not-empty r)
      (let [[nr np _] (parse r)]
        (recur nr (into p np)))
      p)))

(defn part1 [input]
  (let [binary (parse-input input)
        parsed (parse-all binary)]
    (transduce (map :version) + (flatten parsed))))

(defn tf->b [op]
  (comp {true  1
         false 0}
        op))

((tf->b >) 2 1)

(defn evaluate [[{leading-type-id :type-id
                  :as leading} args]]
  (if (= leading-type-id 4)
    leading
    (transduce (map (comp #(select-keys % [:value]) evaluate))
               (partial merge-with (case leading-type-id
                                     0 +
                                     1 *
                                     2 min
                                     3 max
                                     5 (tf->b <)
                                     6 (tf->b >)
                                     7 (tf->b =)))
               args)))

(defn part2 [input]
  (let [binary (parse-input input)
        parsed (parse-all binary)]
    (evaluate parsed)))

(comment
  (part2 "9C0141080250320F1802104A08")
  
  
  )

(println
 "Day 16\n"
 "- Part 1: " (part1 main-input)
 "- Part 2: " (part2 main-input))