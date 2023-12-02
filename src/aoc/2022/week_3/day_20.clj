(ns aoc.2022.week-3.day-20
  (:require [aoc.util :as util]))

(defonce input
  (util/get-input 2022 20))

(def encrypted
  (mapv util/str->int (util/sl input))
  #_(->> input
       util/sl
       (mapv util/str->int)
       (mapv #(conj % (reduce + %)))))

(defn normalize
  [n]
  (cond
    (< n 0) (some #(when (<= 0 % 4999) %)
                  (iterate #(+ 5000 %) n))
    (> n 4999) (some #(when (<= 0 % 4999) %)
                     (iterate #(- % 5000) n))
    :else n))

(defn rearrange
  "Shift one number at index i the amount of its value."
  [c i]
  (let [n (c i)
        before-a (subvec c 0 i)
        before-b (subvec c (inc i) (+ n i 1))
        after (subvec c (+ n i 1))]
    (reduce into [before-a before-b [n] after])))

(rearrange [1 2 3 -4 5 6 7 8 9 10] 3)

(comment
  (loop [enc encrypted]))

;; a a a a a a a 5 a a a a a a a
;; a a a a a a a a a a a a 5 a a

;; 5 at index 7 turns to index 12.
