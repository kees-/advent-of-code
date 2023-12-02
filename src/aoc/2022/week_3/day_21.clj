(ns aoc.2022.week-3.day-21
  (:require [aoc.util :as util]))

(defonce input
  (util/get-input 2022 21))

(defonce sample
  (slurp "assets/21sample.txt"))

(def monkeys
  (let [red (fn [monkey-math monkey]
              (let [[a b c d] monkey]
                (if d
                  (assoc-in monkey-math [:ops a] {:ms [b d] :op (eval (symbol c))})
                  (assoc-in monkey-math [:nums a] (util/str->int b)))))]
    (->> input
         util/sl
         (map #(vec (re-seq #"\w{4}|[\\*\+-\/]|\d+" %)))
         (reduce red {:ops {} :nums {}}))))

(defn replace-with-number
  [nums s]
  (if-let [n (nums s)] n s))

(defn update-monkey
  [nums monkey-op]
  (update monkey-op :ms #(mapv (partial replace-with-number nums) %)))

(defn monkey-layer
  [ops nums]
  (update-vals ops #(update-monkey nums %)))

(defn update-nums
  [nums monkey]
  (let [[mnky {:keys [ms op]}] monkey]
    (if (every? number? ms)
      (let [[a b] ms]
        (assoc nums mnky (op a b)))
      nums)))

(defn monkey-mode
  [monkeys root]
  (let [{:keys [ops nums]} monkeys]
    (loop [ops ops
           nums nums]
      (if-let [n (nums root)]
        n
        (let [ml (monkey-layer ops nums)
              nl (reduce update-nums nums ml)]
          (recur ml nl))))))

(comment
  (monkey-mode monkeys "root"))
