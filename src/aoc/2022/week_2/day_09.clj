(ns aoc.2022.week-2.day-09
  (:require [aoc.util :as util]))

(defonce input
  (util/get-input 2022 9))

(def instructions
  (->> input
       util/sl                                     ; 1) Split lines
       (map (comp (fn [[d n]]
                     (repeat (util/str->int n) d)) ; 3) ["U" 3] -> ("U" "U" "U")
                   #(util/s % #" ")))              ; 2) Split instr on space
       (reduce into [])                            ; 4) Flatten
       (map {"U" [0 1]                             ; 5) Direction -> coord
             "D" [0 -1]
             "L" [-1 0]
             "R" [1 0]})))

(def offsets
  {[-2 2]  [-1 1]
   [-2 1]  [-1 1]
   [-2 0]  [-1 0]
   [-2 -1] [-1 -1]
   [-2 -2] [-1 -1]

   [-1 2]  [-1 1]
   [-1 -2] [-1 -1]
   [0 2]   [0 1]
   [0 -2]  [0 -1]
   [1 2]   [1 1]
   [1 -2]  [1 -1]

   [2 2]   [1 1]
   [2 1]   [1 1]
   [2 0]   [1 0]
   [2 -1]  [1 -1]
   [2 -2]  [1 -1]})

(defn find-offset
  "Compare two coordinates and return how `base` will shift."
  [base moved]
  (if-let [offset (offsets (mapv - moved base))]
    offset
    [0 0]))

(defn step
  "Increment each position of linked coordinates
   based on an initial translation."
  [rope dir]
  (loop [d dir
         acc []
         [head & remaining] rope]
    (if head
      (let [h (mapv + head d)]
        (recur (find-offset (first remaining) h)
               (conj acc h)
               remaining))
      acc)))

(defn parse
  "Iterate all translations of a series of coordinates,
   as described by a sequence of UDLR movements."
  [length instructions]
  (loop [rope (vec (repeat length [0 0]))
         visited #{}
         [dir & remaining] instructions]
    (if dir
      (let [new-rope (step rope dir)]
        (recur new-rope
               (conj visited (last new-rope))
               remaining))
      (count visited))))
;; This would work for arbitrary translations and offset rulesets,
;; but with a static offset map every case would have to be covered

(comment
  (parse 2 instructions)
  ;; => 6266
  (parse 10 instructions)
  ;; => 2369
  )
