(ns aoc2017.day2
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(defn- make-soln
  [line-calc]
  (fn
    [spreadsheet]
    (->> (str/split spreadsheet #"\n")
         (map #(str/split % #"\t"))
         (map line-calc)
         (apply +))))

(defn- line-reducer
  [p next]
  (loop [high (first p)
         low (second p)]
    (cond
      (nil? low) [next next]
      (> low next) (recur high next)
      (< high next) (recur next low)
      :else [high low])))

(defn- calculate-line
  [line]
  (->> line
       (map #(Integer/parseInt %))
       (reduce line-reducer [nil nil])
       (apply -)))

(defn- divides?
  [[a b]]
  (cond
    (zero? (mod a b)) (/ a b)
    (zero? (mod b a)) (/ b a)))

(defn- calculate-div
  [line]
  (as-> line v
        (map #(Integer/parseInt %) v)
        (combo/combinations v 2)
        (map divides? v)
        (some identity v)))

(def soln1 (make-soln calculate-line))
(def soln2 (make-soln calculate-div))
