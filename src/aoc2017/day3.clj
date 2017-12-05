(ns aoc2017.day3)

(defn- ring-number
  [num]
  (as-> num %
        (Math/sqrt %)
        (Math/ceil %)
        (int %)
        (if (even? %)
          (inc %)
          %)
        (quot % 2)))

(defn- max-in-ring
  [rn]
  (-> rn
      (* 2)
      (inc)
      (Math/pow 2)
      (int)))

(defn- dist-in-ring
  [n]
  (let [rn (ring-number n)
        m (max-in-ring rn)
        i (* 2 rn)
        start (- m rn)
        diff-to-next (-> (- n start)
                         (Math/abs)
                         (mod i))]
    (min (- i diff-to-next)
         (- diff-to-next 0))))

(defn soln1
  [n]
  (+ (ring-number n)
     (dist-in-ring n)))
