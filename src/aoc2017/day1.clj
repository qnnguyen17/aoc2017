(ns aoc2017.day1)

(defn- rotate
  "Creates a new string where the first char is now at the end."
  [string]
  (str
   (subs string 1)
   (first string)))

(defn- rotate-half
  "Creates a new string rotated by half its original length"
  [string]
  (let [half (/ (count string) 2)]
    (str
     (subs string half)
     (subs string 0 half))))

(defn- compare-digits
  "Does the digit-comparison and outputs the right value."
  [ch1 ch2]
  (if (= ch1 ch2)
    (Character/digit ch1 10)
    0))

(defn- make-soln
  "Creates the solution based on a rotation function"
  [rotation-fn]
  (fn
    [digits]
    (->> (rotation-fn digits)
         (map compare-digits digits)
         (apply +))))

(def soln1 (make-soln rotate))

(def soln2 (make-soln rotate-half))
