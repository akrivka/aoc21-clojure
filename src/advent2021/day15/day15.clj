(ns advent2021.day15
  (:require [clojure.string :as str]
            [advent2021.util :as u]))

(def main-input (slurp "src/advent2021/day15/day15.in"))

(def test-input (slurp "src/advent2021/day15/day15.test.in"))

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map #(map (comp parse-long str) %))
       (map vec)
       (vec)))

(defn get-xy [m [x y]]
  (get-in m [y x]))

(defn assoc-xy [m [x y] v]
  (assoc-in m [y x] v))

(defn update-xy [m [x y] f]
  (update-in m [y x] f))


(defn dmap [f m]
  (map #(map f %) m))

(defn dmapv [f m]
  (mapv #(mapv f %) m))

(defn dmap-indexed [f m]
  (map-indexed (fn [y r] (map-indexed (fn [x e] (f [x y] e)) r)) m))

(defn dreduce [f i m]
  (let [[w h] [(count (first m)) (count m)]]
    (reduce f i (for [x (range w)
                      y (range h)]
                  [x y]))))

(defn dfilter [pred m]
  (let [[w h] [(count (first m)) (count m)]]
    (for [x (range w)
          y (range h)
          :when (pred (get-xy m [x y]))]
      [x y])))

(def infinity 100000000000)

(defn get-neighbors [matrix [x y]]
  (keep #(when (get-xy matrix %) %)
        [[(inc x) y] [x (inc y)] [(dec x) y] [x (dec y)]]))

(defn visit [matrix costs visited v]
  (let [unvisited-neighbors (filter #(not (get-xy visited %)) (get-neighbors matrix v))
        current-cost        (get-xy costs v)]
    (reduce
     (fn [cs n]
       (let [oc (get-xy costs n)
             nc (+ current-cost (get-xy matrix n))]
         (if (< nc oc)
           (assoc-xy cs n nc)
           cs)))
     costs unvisited-neighbors)))

(defn find-minimal-unvisited [costs visited]
  (let [unvisited (dfilter false? visited)]
    (when (not-empty unvisited) (apply min-key #(get-xy costs %) unvisited))))

(defn find-lowest-total-risk [matrix a b]
  (loop [costs   (assoc-xy (dmapv (constantly infinity) matrix) a 0)
         visited (dmapv (constantly false) matrix)
         current [0 0]]
    (let [new-costs   (visit matrix costs visited current)
          new-visited (assoc-xy visited current true)
          next        (find-minimal-unvisited new-costs new-visited)]
      (if (= current b)
        (get-xy costs b)
        (recur new-costs
               new-visited
               next)))))

(take 5 [1 2 3 4 5])

(defn insert-into-sorted [coll x]
  (loop [coll1 coll
         coll2 []]
    (let [a1 (first coll1)
          a2 (peek coll2)]
      (if (and (compare x a2) (compare a1 x))
        (into [] (concat coll2 [x] coll1))
        (recur (next coll1) (conj coll2 a1))))))

(defn insert-into-sorted-by [keyfn x coll]
  (if (empty? coll) 
    [x]
  (loop [coll1 coll
         coll2 []]
    (let [a1 (first coll1)
          a2 (peek coll2)]
      (if (or (nil? a1)
              (every? (comp not neg? #(apply compare %) #(map keyfn %)) [[x a2] [a1 x]]))
        (into [] (concat coll2 [x] coll1))
        (recur (next coll1) (conj coll2 a1)))))))

(defn remove-index [coll i]
  (into [] (concat (take i coll) (drop (inc i) coll))))

(defn find-lowest-total-risk2 [matrix a b]
  (loop [[[cur-xy cur-cost] & queue]    (conj (for [x (range (count (first matrix)))
                                                    y (range (count matrix))
                                                    :when (not= [x y] a)]
                                                [[x y] infinity]) [a 0])
         visited #{}]
    (if (= cur-xy b)
      cur-cost
      (let [unvisited-neighbors (filter #(not (visited %)) (get-neighbors matrix cur-xy))
            new-queue           (reduce (fn [q n]
                                          (let [[i oc] (some (fn [[i [v c]]] (when (= v n) [i c])) (map-indexed vector q))
                                                nc     (+ cur-cost (get-xy matrix n))]
                                            (if (< nc oc)
                                              (insert-into-sorted-by second [n nc] (remove-index q i))
                                              q)))
                                        queue
                                        unvisited-neighbors)
            ]
        (recur (remove #(= (first %) cur-xy) new-queue)
               (conj visited cur-xy))))))


(defn part1 [input]
  (let [cave  (parse-input input)
        [w h] [(count (first cave)) (count cave)]
        risk  (find-lowest-total-risk2 cave [0 0] [(dec w) (dec h)])]
    risk))

(defn join-caves-x [c1 c2]
  (mapv #(into [] (concat %1 %2)) c1 c2))

(defn join-caves-y [c1 c2]
  (into [] (concat c1 c2)))

(defn bump-cave [cave]
  (dmapv #(let [n (inc %)]
            (if (> n 9) 1 n)) cave))

(defn expand-cave [cave [k l]]
  (let [horiz-ex (reduce join-caves-x (take k (iterate bump-cave cave)))
        full-ex  (reduce join-caves-y (take l (iterate bump-cave horiz-ex)))]
    full-ex))

(defn part2 [input]
  (let [initial-cave  (parse-input input)
        cave (expand-cave initial-cave [5 5])
        [w h] [(count (first cave)) (count cave)]
        risk  (find-lowest-total-risk2 cave [0 0] [(dec w) (dec h)])]
    risk))

(comment
  (time (part1 main-input))
  
  
  )

(println "Day 15")
(println "- Part 1 (test): " (time (part1 test-input)))
(println "- Part 2 (test): " (time (part2 test-input)))
(println "- Part 1 (main): " (time (part1 main-input)))
(println "- Part 2 (main): " (time (part2 main-input)))