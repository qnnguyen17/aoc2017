(ns aoc2017.day5
  (:require [clojure.string :as str]))

(defn- make-soln
  [soln]
  (fn [filename]
    (as-> filename $
          (slurp $)
          (str/split $ #"\n")
          (map #(Integer/parseInt %) $)
          (into [] $)
          (soln $))))

(def soln1
  (make-soln
   (fn [offsets]
     (loop [pos 0
            o offsets
            steps 0]
       (if-let [jump (nth o pos nil)]
         (recur (+ pos jump)
                (update o pos inc)
                (inc steps))
         steps)))))

(def soln2
  (make-soln
   (fn [offsets]
     (loop [pos 0
            o offsets
            steps 0]
       (if-let [jump (nth o pos nil)]
         (recur (+ pos jump)
                (if (>= jump 3)
                  (update o pos dec)
                  (update o pos inc))
                (inc steps))
         steps)))))
