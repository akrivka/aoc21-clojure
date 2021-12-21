(ns advent2021.day20
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :refer [expt]]))

(def main-input (slurp "src/advent2021/day20/day20.in"))

(def test-input (slurp "src/advent2021/day20/day20.test.in"))

(defn parse-input [input]
  (let [[alg image] (str/split input #"\n\n")]
    [(vec alg) (mapv vec (str/split-lines image))]))

(defn extend-image [image pixel]
  (let [w      (count (first image))
        empty-row  (repeat (+ 2 w) pixel)]
    (into []
          (concat
           [empty-row]
           (map #(into [] (concat [pixel] % [pixel])) image)
           [empty-row]))))

(defn dmapv [f m]
  (mapv #(mapv f %) m))

(defn dmap-indexed [f m]
  (map-indexed (fn [y r] (map-indexed (fn [x e] (f [x y] e)) r)) m))

(defn get-xy [m [x y]]
  (get-in m [y x]))

(defn dreduce [f i m]
  (let [[w h] [(count (first m)) (count m)]]
    (reduce f i (for [x (range w)
                      y (range h)]
                  [x y]))))

(defn binary->decimal [bs]
  (second
   (reduce
    (fn [[e r] b]
      [(inc e) (if (= b 1)
                 (+ r (expt 2 e))
                 r)])
    [0 0]
    (reverse bs))))


(defn enhance
  ([alg image] (enhance alg image \.))
  ([alg image inf-pixel]
   (let [prepared-image (extend-image image inf-pixel)
         new-image      (dmap-indexed
                         (fn [[x y] _]
                           (as-> prepared-image $
                             (for [yy [(dec y) y (inc y)]]
                               (for [xx [(dec x) x (inc x)]]
                                 (or (get-xy $ [xx yy]) inf-pixel)))
                             (apply concat $)
                             (map {\. 0 \# 1} $)
                             (binary->decimal $)
                             (get alg $)))
                         prepared-image)
         new-inf-pixel  (->> (repeat 9 inf-pixel)
                             (map {\. 0 \# 1})
                             (binary->decimal)
                             (get alg))]
     [new-image new-inf-pixel])))

(defn string-image [image]
  (->> image
       (map #(str/join #"" %))
       (str/join "\n")))

(defn part1 [input]
  (let [[alg image] (parse-input input)
        enhance'    (partial enhance alg)
        twice-enhanced (enhance' (enhance' image) \#)]
    (frequencies (flatten twice-enhanced))))

(defn part2 [input n]
  (let [[alg image] (parse-input input)
        enhance'    #(apply (partial enhance alg) %)
        twice-enhanced (nth (iterate enhance' [image \.]) n)]
    (frequencies (flatten twice-enhanced))))

(comment
  (part2 main-input 50)
  ;;
  )

(println
 "Day 20\n"
 "- Part 1: " (part1 main-input)
 "- Part 2: " (part2 main-input))