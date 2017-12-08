(ns aoc2017.day8
  (:require [clojure.string :as str]))

(defn- parse-op
  "Convert the operator string into a fn"
  [op]
  (case op
    "inc" +
    "dec" -))

(defn- parse-cmp
  "Convert the comparator string into a fn"
  [cmp]
  (case cmp
    ">=" >=
    "<=" <=
    "==" =
    "!=" not=
    "<" <
    ">" >))

(defn- parse
  [in-str]
  (let [[op-reg op op-arg cmp-reg cmp cmp-arg]
        (rest (re-find #"([a-z]+) (inc|dec) (-?\d+) if ([a-z]+) (>=|<=|==|!=|>|<) (-?\d+)"
               in-str))]
    {:op-reg op-reg
     :op (parse-op op)
     :op-arg (Integer/parseInt op-arg)
     :cmp-reg cmp-reg
     :cmp (parse-cmp cmp)
     :cmp-arg (Integer/parseInt cmp-arg)}))

(defn- reduce-instrs
  [regs {:keys [op-reg op op-arg cmp-reg cmp cmp-arg]}]
  (let [op-reg-val (get regs op-reg 0)
        cmp-reg-val (get regs cmp-reg 0)
        max-val (get regs :max)]
    (if (cmp cmp-reg-val cmp-arg)
      (let [new-reg-val (op op-reg-val op-arg)]
        (assoc regs
               op-reg (op op-reg-val op-arg)
               :max (if (or (not max-val)
                            (> new-reg-val max-val))
                      new-reg-val
                      max-val)))
      regs)))

(defn- make-soln
  [val-selector]
  (fn [in-file]
    (as-> in-file $
          (slurp $)
          (str/split $ #"\n")
          (map parse $)
          (reduce reduce-instrs {} $)
          (val-selector $))))

(def soln1
  (make-soln #(as-> % $
                    (dissoc $ :max)
                    (vals $)
                    (apply max $))))

(def soln2 (make-soln :max))
