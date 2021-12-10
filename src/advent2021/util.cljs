(ns advent2021.util
  (:require [clojure.set :as set]))

(defn node-slurp [path]
  (let [fs (js/require "fs")]
    (.readFileSync fs path "utf8")))

(defn abs [n]
  (max n (- n)))

(defn exp [x n]
  (reduce * (repeat n x)))

(defn single [xs]
  (assert (= (count xs) 1))
  (first xs))

(defn w-some [f coll]
  (some #(when (f %) %) coll))