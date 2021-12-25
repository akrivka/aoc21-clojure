(ns advent2021.day24
  (:require [clojure.string :as str]
            [advent2021.util :as u]))

(def monad (slurp "src/advent2021/day24/monad.in"))

(def monad-alt (slurp "src/advent2021/day24/monad-alt.in"))

(def negate "inp x
mul x -1")

(def three-times "inp z
inp x
mul z 3
eql z x")

(def binary "inp w
add z w
mod z 2
div w 2
add y w
mod y 2
div w 2
add x w
mod x 2
div w 2
mod w 2")

(defn string-state [{:keys [w x y z]}]
  (str "w:" w ", x:" x ", y:" y ", z:" z))

(defn parse-program [program]
  (->> program
       (str/split-lines)
       (map (fn [line] (let [[ins' & args'] (str/split line #" ")
                             ins (keyword ins')
                             args (map #(cond
                                          (#{"w" "x" "y" "z"} %) (keyword %)
                                          :else (parse-long %)) args')]
                         (concat [ins] args))))))

(defmulti process (fn [_ [op]] op))

(defmethod process :default [& args] (println "Invalid arguments " args))

(defmethod process :inp [{:keys [input] :as state} [_ a]]
  #_(println "rem:" (count input) " " (string-state state))
  (let [number (first input)]
    (-> state
        (assoc :input (rest input))
        (update :solution conj number)
        (assoc a number))))

(defn arithmetic-op [state [opfun a b]]
  (let [bv (cond
             (keyword? b) (state b)
             (number? b)  b)
        av (state a)
        result (opfun av bv)]
    (assoc state a result)))

(defmethod process :add [state [_ a b]]
  (arithmetic-op state [+ a b]))

(defmethod process :mul [state [_ a b]]
  (arithmetic-op state [* a b]))

(defmethod process :div [state [_ a b]]
  (arithmetic-op state [quot a b]))

(defmethod process :mod [state [_ a b]]
  (arithmetic-op state [mod a b]))

(defmethod process :eql [state [_ a b]]
  (arithmetic-op state [#(if (= %1 %2) 1 0) a b]))

(defn run-program
  ([program input] (run-program program input {:w 0 :x 0 :y 0 :z 0
                                               :input input}))
  ([program _ initial-state]
   (loop [state             (transient initial-state)
          [fline & rlines]  program
          i 0]
     (if fline
       (recur (process state fline) rlines (inc i))
       (persistent! state)))))

(defn decimal->digits [n]
  (->> n
       str
       seq
       (map (comp parse-long str))))

(defn digits->decimal [digits]
  (->> digits
       (str/join "")
       parse-long))

(defn part1 []
  (let [monad-program (parse-program monad)]
    (loop [n (digits->decimal (repeat 14 9))]
      (let [input (decimal->digits n)]
        (when (= (mod n 10000) 0) (println n))
        (if (some #(= % 0) input)
          (recur (dec n))
          (let [{:keys [z]} (run-program monad-program input)]
            (if (= z 0)
              n
              (recur (dec n)))))))))

(defn pad-with [n k coll]
  (let [p (- k (count coll))]
    (concat (repeat p n) coll)))

(def all (range 1 10))

(defn experiment1 []
  (let [monad-program (parse-program monad)
        inputs (for [d14 all
                     d13 all
                     d12 all
                     d11 all
                     d10 all
                     d9  all
                     d8  [9]
                     d7  [9]
                     d6  [9]
                     d5  [9]
                     d4  [9]
                     d3  [9]
                     d2  [9]
                     d1  [9]]
                 [d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14])
        inputs2 (for [d14 [6 7]
                      d1 all
                      d2 [1 2]]
                  [d1 d2 1 4 9 9 6 1 8 9 4 9 6 d14])
        results (mapv
                 #(let [{:keys [z] :as result}  (run-program monad-program %)]
                    (when (< z 200000000000)
                      #_(println (string-state result))))
                 inputs)]
    results))

(defn experiment2' [initial-state parts]
  (loop [state initial-state
         [npart & rparts]     parts
         solution []]
    (let [results (->> all
                       (map #(run-program npart nil (assoc state :input [%])))
                       (filter (fn [{:keys [x y z]}] (every? #(= % 0) [x y z]))))]
      (if (empty? results)
        ()))))

(defn experiment2 []
  (let [parts (map parse-program (str/split monad-alt #"\n\n"))]
    (experiment2' {:w 0 :x 0 :y 0 :z 0} parts)))

(defn experiment3 []
  (loop [state {:w 0 :x 0 :y 0 :z 0}
         [npart & rparts] (map parse-program (str/split monad-alt #"\n\n"))
         solution []]
    (if (= (count solution) 14)
      [solution state]
      (let [[next-digit next-state] (->> [1 2 3 4 5 6 7 8 9]
                                         (map (fn [d] [d (run-program npart nil (assoc state :input [d]))]))
                                         (group-by (comp :z second))
                                         (sort-by first)
                                         (u/spy)
                                         first second first)]
        (recur next-state rparts (conj solution next-digit))))))

(defn experiment4 []
  (let [sol [9 9 6 9 9 3 9 9 9 9 4 6]]
    (loop [[nsol & rsol]    sol
           [npart & rparts] (take (inc (count sol))
                                  (map parse-program (str/split monad-alt #"\n\n")))
           state {:w 0 :x 0 :y 0 :z 0}]
      (if-not nsol
        (map #(run-program npart nil (assoc state :input [%])) [1 2 3 4 5 6 7 8 9])
        (recur rsol rparts (run-program npart nil (assoc state :input [nsol])))))))

(defn run-part-with
  ([state part ninput]
   (loop [state            (assoc state :input [ninput])
          [fline & rlines]  part
          first-eql true]
     (if fline
       (let [next-state (process state fline)]
         (if (= (first fline) :eql)
           (when (or (not first-eql) (= (:x next-state) 1))
             (recur next-state rlines false))
           (recur next-state rlines first-eql)))
       state))))

(defn experiment5' [state [npart & rparts]]
  (if npart
    (let [r (keep
             #(when-let [next-state (run-part-with state npart %)]
                (experiment5' next-state rparts))
             [1 2 3 4 5 6 7 8 9])]
      (when (not-empty r) (flatten r)))
    state))

(seq '("a"))

(defn experiment5 []
  (let [parts (map parse-program (str/split monad-alt #"\n\n"))]
    (experiment5' {:w 0 :x 0 :y 0 :z 0 :solution []} parts)))

(defn part2 [input])

(comment
  (run-program monad [1 1 1 1 1 1 1 1 1 1 1 1 1 1])
  (part1)
  (time (experiment1))
  (experiment2)
  (map :z (experiment4))
  (apply min
         (map (comp digits->decimal :solution) (experiment5)))
  ;;
  )

(println
 "Day 24\n"
 "- Part 1: " (part1)
 "- Part 2: " #_(part2 main-input))