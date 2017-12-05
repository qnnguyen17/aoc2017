(ns aoc2017.day4
  (:require [clojure.string :as str]))

(defn- is-valid-phrase-exact?
  [phrase]
  (second (reduce
           (fn
             [[words valid?] w]
             (cond
               (not valid?) [words false]
               (words w) [words false]
               :else [(conj words w) true]))
           [#{} true]
           (str/split phrase #"\s"))))

(defn- sort-word
  [w]
  (apply str (sort (seq w))))

(defn- is-valid-phrase-anagram?
  [phrase]
  (second (reduce
           (fn
             [[words valid?] w]
             (cond
               (not valid?) [words false]
               (words (sort-word w)) [words false]
               :else [(conj words (sort-word w)) true]))
           [#{} true]
           (str/split phrase #"\s"))))

(defn- make-solution
  [check-fn]
  (fn [phrases]
    (as-> phrases %
          (str/split % #"\n")
          (map check-fn %)
          (filter identity %)
          (count %))))

(def soln1 (make-solution is-valid-phrase-exact?))

(def soln2 (make-solution is-valid-phrase-anagram?))
