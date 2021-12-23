(ns advent2021.day21
  (:require [clojure.string :as str]
            [advent2021.util :as u]))

(def main-input (slurp "src/advent2021/day21/day21.in"))

(def test-input (slurp "src/advent2021/day21/day21.test.in"))

(defn parse-input [input]
  (->> input))

;; ---

(def main [3 10])

(def test [4 8])

(defn mod' [n m]
  (let [orig (mod n m)]
    (if (= orig 0)
      (+ orig m)
      orig)))


(take 3 (iterate inc 1))

(defn play-until-win [starting-players]
  (loop [{:keys [i dice-state players-turn players] :as state}
         {:i 1
          :dice-state 1
          :players-turn 0
          :players (mapv (fn [s] {:pos s
                                  :sco 0}) starting-players)}]
    (let [dice-roll (apply + (take 3 (iterate (comp #(mod' % 100) inc) dice-state)))
          new-players (update players players-turn
                              (fn [{:keys [pos sco]}]
                                (let [new-pos (mod' (+ pos dice-roll) 10)]
                                  {:pos new-pos
                                   :sco (+ sco new-pos)})))]
      (if (some #(>= (:sco %) 1000) new-players)
        state
        (recur {:i (inc i)
                :dice-state (+ 3 dice-state)
                :players-turn (mod (inc players-turn) 2)
                :players new-players})))))

(defn part1 [starting-players]
  (let [{:keys [i players]} (play-until-win starting-players)]
    (* (* i 3)
       (apply min (map :sco players)))))

(def quant-dice [1 2 3])

(defn simulate-old [[{pos1 :pos
                      sco1 :sco
                      :as p1}
                     {pos2 :pos
                      sco2 :sco
                      :as p2}
                     :as players]]
  (if-let [winner (some #(when (>= (:sco %) 21) (:id %)) players)]
    {winner 1}
    (reduce (partial merge-with +)
            (for [d1a quant-dice
                  d1b quant-dice
                  d1c quant-dice
                  d2a quant-dice
                  d2b quant-dice
                  d2c quant-dice]
              (let [new-pos1 (mod' (+ pos1 d1a d1b d1c) 10)
                    new-pos2 (mod' (+ pos2 d2a d2b d2c) 10)]
                (simulate-old [(merge p1 {:pos new-pos1
                                          :sco (+ sco1 new-pos1)})
                               (merge p2 {:pos new-pos2
                                          :sco (+ sco2 new-pos2)})]))))))

(def dice-freqs
  (frequencies
   (for [a quant-dice
         b quant-dice
         c quant-dice]
     (+ a b c))))

(defn get-next-universes [{:keys [players turn]}]
  (into {}
        (map
         (fn [[roll freq]]
           (let [new-turn (mod (inc turn) 2)
                 new-players (into [] (update players turn (fn [{:keys [pos sco]}]
                                                             (let [new-pos (mod' (+ pos roll) 10)]
                                                               {:pos new-pos
                                                                :sco (+ sco new-pos)}))))]
             [{:turn new-turn 
               :players new-players} freq])))
        dice-freqs))

(defn prune-universes [universes]
  (reduce
   (fn [agg [{:keys [players] :as universe} freq]]
     (if-let [winner (some (fn [[i {:keys [sco]}]] (when (>= sco 21) i)) (map-indexed vector players))]
       (update-in agg [:new-winners winner] + freq)
       (assoc-in agg [:active-universes universe] freq)))
   {:active-universes {}
    :new-winners {0 0
                  1 0}}
   universes))

(defn simulate [starting-state]
  (loop [universes {starting-state 1}
         winners   {0 0
                    1 0}]
    (let [new-universes (reduce (fn [new-universes [universe freq]]
                                  (merge-with +
                                              new-universes
                                              (->> universe
                                                   get-next-universes
                                                   (map (fn [[k v]] [k (* v freq)]))
                                                   (into {}))))
                                {}
                                universes)
          {:keys [active-universes new-winners]} (prune-universes new-universes)
          all-winners (merge-with + winners new-winners)]
      (println winners)
      (if (not-empty active-universes)
        (recur active-universes all-winners)
        all-winners))))

;; maybe i can just keep a list of the universes together with the number of times they occur
;; so a map from a possible universe state to how many there are
;; whenever a universe-type ends, its "frequency" is added to a global map of frequencies of when who wins
;; and the universe is dissoc-ed from the list of universes, so it's not simulated further
;; does this improve performance?
;; yes, because i only need to keep track of all possible universe states, which is not that much
;; and it saves doing a lot of computations multiple times
;; can it get any better?
;; probably not since going from one unique state to another really needs to be always calculated, there doesn't seem to be any shortcut
;; or?
;; ---
;; is this recursive or just a loop? 
;; probably a loop...

(defn part2 [[s1 s2]]
  (simulate {:turn 0
             :players [{:pos s1
                        :sco 0}
                       {:pos s2
                        :sco 0}]}))

(comment
  (part2 main)
  ;;
  )

(println
 "Day 21\n"
 "- Part 1: " (time (part1 main))
 "- Part 2: " (time (part2 test)))