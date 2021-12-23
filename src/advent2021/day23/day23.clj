(ns advent2021.day23
  (:require [clojure.string :as str]
            [advent2021.util :as u]
            [clojure.math.numeric-tower :refer [abs]]))

(def main 
  (sorted-map
   1  nil
   2  nil
   3  [:D :D]
   4  nil
   5  [:A :C]
   6  nil
   7  [:C :B]
   8  nil
   9  [:A :B]
   10 nil
   11 nil))

(def main2 
  (sorted-map
   1  nil
   2  nil
   3  [:D :D :D :D]
   4  nil
   5  [:A :C :B :C]
   6  nil
   7  [:C :B :A :B]
   8  nil
   9  [:A :A :C :B]
   10 nil
   11 nil))

(def example
  (sorted-map
   1  nil
   2  nil
   3  [:B :A]
   4  nil
   5  [:C :D]
   6  nil
   7  [:B :C]
   8  nil
   9  [:D :A]
   10 nil
   11 nil))

(def example2
  (sorted-map
   1  nil
   2  nil
   3  [:B :D :D :A]
   4  nil
   5  [:C :C :B :D]
   6  nil
   7  [:B :B :A :C]
   8  nil
   9  [:D :A :C :A]
   10 nil
   11 nil))


;; map from states to the least energy to reach them found so far
;; start at the starting-state, find all possible future states, and note the energy to reach them 
;; (i.e. again a map from states to energies)
;; then for each of the new states, find all next possible states possible states, the energy to reach them plus the energy to reach the current state
;; if a state has been considered already, compare the energy to reach it and only store the lower value

(def amphod->energy-per-step
  {:A 1
   :B 10
   :C 100
   :D 1000})

;; if there's any amphod that can move into its room, move them
;; for each room, check if it's movable
;; then loop over all amphos in the hallway and check if anyone can move into the room
;; then loop over the amphods in the other three rooms

(defn position-to-room? [state i r]
  (every?
   #(nil? (state %))
   (remove (set [3 5 7 9 r i]) (range (min i r) (max i r)))))

(defn locs->energy [i r a amp]
  (*
   (amphod->energy-per-step amp)
   (+ (inc a) (abs (- r i)))))

(defn finish-state [[state energy]]
  (loop [[[r amp] & rrs] [[3 :A] [5 :B] [7 :C] [9 :D]]]
    (if (nil? r)
      [state energy]
      (if (some #(when % (not= % amp)) (state r))
        (recur rrs)
        (or
         (when-let [i (some #(when (and (= (state %) amp)
                                        (position-to-room? state % r)) %) [1 2 4 6 8 10 11])]
           (let [queue (state r)
                 a (dec (or (u/index-of some? queue) (count queue)))
                 queue' (assoc queue a amp)
                 state' (-> state
                            (assoc i nil)
                            (assoc r queue'))]
             (finish-state [state'
                            (+ energy
                               (locs->energy i r a amp))])))
         (when-let [[i a'] (some #(let [queue (state %)
                                        a (u/index-of some? queue)]
                                    (when (and a
                                               (= (nth queue a) amp)
                                               (position-to-room? state % r))
                                      [% a])) (disj #{3 5 7 9} r))]
           (let [queue (state r)
                 a (dec (or (u/index-of some? queue) (count queue)))
                 queue' (assoc queue a amp)
                 state' (-> state
                            (assoc-in [i a'] nil)
                            (assoc r queue'))]
             (finish-state [state'
                            (+ energy
                               (locs->energy i r (+ a' a 1) amp))])))
         (recur rrs))))))

(defn find-all-possible-next-states [state]
  (apply merge-with min
         (mapcat
          (fn [[r tamp]]
            (let [queue  (state r)
                  a      (u/index-of some? queue)]
              (when-not (every? #(or (= % tamp) (nil? %)) queue)
                (let [lb (loop [i (inc r)] (let [ni (- i (if (#{3 5 7 9} (dec i)) 2 1))]
                                             (cond
                                               (= ni 0)   1
                                               (state ni) i
                                               :else (recur ni))))
                      rb (loop [i (dec r)] (let [ni (+ i (if (#{3 5 7 9} (inc i)) 2 1))]
                                             (cond
                                               (= ni 12)   11
                                               (state ni) i
                                               :else (recur ni))))]
                  (when (and (<= lb rb) a)
                    (let [moves  (remove #{3 5 7 9} (range lb (inc rb)))
                          amp    (nth queue a)
                          queue' (assoc queue a nil)
                          state' (assoc state r queue')]
                      (map (comp
                            #(into {} [%])
                            finish-state
                            (fn [i] [(assoc state' i amp)
                                     (locs->energy i r a amp)])) moves)))))))
          [[3 :A] [5 :B] [7 :C] [9 :D]])))

(defn string-state [state]
  (str
   (apply str (map (fn [[i v]]
                     (if (or (#{3 5 7 9} i)
                             (nil? v))
                       "•"
                       (name v))) state))
   "\n"
   (str/join "\n"
             (apply
              map
              (fn [& l] (str "##" (apply str (interpose "#" (map #(if % (name %) "•") l))) "##"))
              (map #(state %) [3 5 7 9])))))

(defn print-pair [[state energy :as a]]
  (when (and state energy)
    (println
     (string-state state)
     energy
     "\n-----------")))

(defn is-final? [state]
  (every? 
   (fn [[r tamp]]
     (let [queue (state r)]
       (every? #(= % tamp) queue)))
   [[3 :A] [5 :B] [7 :C] [9 :D]]))

(defn find-least-energy [starting-state]
  (loop [states {starting-state 0}]
    (if (and (= (count states) 1) (is-final? (first (first states))))
      (print-pair (first states))
      (recur (apply merge-with min
                    (map (fn [[state energy]]
                           (if (is-final? state)
                             {state energy}
                             (into {}
                                   (map (fn [[k v]] [k (+ v energy)]))
                                   (find-all-possible-next-states state)))) states))))))

(comment
  (find-least-energy main2)
  ;;
  )

(println
 "Day 23\n"
 "- Part 1: " (find-least-energy main)
 "- Part 2: " (find-least-energy main2))