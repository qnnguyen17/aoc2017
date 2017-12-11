(ns aoc2017.day9
  (:require [clojure.string :as str]))

(defn- next-valid-index
  "Returns the index of the next non-cancelled char after the current index"
  [all-chars idx]
  (let [next-idx (inc idx)]
    (if (= \! (nth all-chars next-idx nil))
      (recur all-chars (inc next-idx))
      next-idx)))

(defn- next-non-garbage-index
  [all-chars idx]
  (let [next-idx (next-valid-index all-chars idx)]
    (if (= \> (nth all-chars next-idx))
      (inc next-idx)
      (recur all-chars next-idx))))

(defn- score-groups
  [all-chars level total curr-idx]
  (let [curr-chr (nth all-chars curr-idx nil)]
    (case curr-chr
      nil total ; End of string
      \, (recur all-chars level total (next-valid-index all-chars curr-idx))
      \{ (recur all-chars (inc level) total (next-valid-index all-chars curr-idx))
      \} (recur all-chars (dec level) (+ total level) (next-valid-index all-chars curr-idx))
      \< (recur all-chars level total (next-non-garbage-index all-chars curr-idx)))))

(defn- count-garbage-chars
  [all-chars idx]
  (loop [ac all-chars
         i idx
         count 0]
    (let [next-idx (next-valid-index ac i)]
      (if (= \> (nth ac next-idx))
        [count (inc next-idx)]
        (recur ac next-idx (inc count))))))

(defn- count-garbage
  [all-chars total curr-idx]
  (let [curr-chr (nth all-chars curr-idx nil)]
    (case curr-chr
      nil total ; End of string
      \, (recur all-chars total (next-valid-index all-chars curr-idx))
      \{ (recur all-chars total (next-valid-index all-chars curr-idx))
      \} (recur all-chars total (next-valid-index all-chars curr-idx))
      \< (let [[c garbage-end] (count-garbage-chars all-chars curr-idx)]
           (recur all-chars (+ total c) garbage-end)))))

(defn soln1
  [file]
  (-> file
      (slurp)
      (str/trim)
      (score-groups 0 0 0)))

(defn soln2
  [file]
  (-> file
      (slurp)
      (str/trim)
      (count-garbage 0 0)))
