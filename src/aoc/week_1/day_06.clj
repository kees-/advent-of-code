(ns aoc.week-1.day-06
  (:require [aoc.util :as util]))

(defonce input
  (util/get-input 6))

(defn start-of-message
  [s n]
  (let [regex (->> (partition n 1 s)
                   (some #(when (apply distinct? %) %))
                   (reduce str "(.*)")
                   re-pattern)]
    (->> s (re-find regex) first count)))

(start-of-message input 4)
(start-of-message input 14)
