(ns aoc.2022.week-3.day-15
  (:require [aoc.util :as util]))

(defonce input
  (util/get-input 2022 15))

(def coords
  (->> input
       util/sl
       (mapv (comp #(mapv util/str->int %)
                   #(re-seq #"\d+" %)))))

(defn manhattan
  "Manhattan distance between two points represented in a single vector."
  [[x1 y1 x2 y2]]
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))

(defn manhattan-row-interval
  "Find the two points on a horizontal line that have
   the manhattan distance from x1,y1 calculated by two coords [x1 y1 x2 y2]"
  [row [x y & _ :as coord]]
  (let [mhtn (manhattan coord)
        dist (abs (- y row))]
    (when (<= dist mhtn)
      (let [w (- mhtn dist)]
        (->> [(+ x w) (- x w)] sort)))))

;; https://www.mail-archive.com/clojure@googlegroups.com/msg49813.html
(defn merge-intervals
  "Given a sorted seq of overlapping intervals, merge, removing their overlaps."
  [acc interval]
  (let [[this & remaining] acc
        intersect? (fn [[s1 e1] [s2 e2]]
                     (not (or (> s1 e2) (> s2 e1))))
        join (fn [[s1 e1] [s2 e2]]
               [(min s1 s2) (max e1 e2)])]
    (if (and this (intersect? this interval))
      (cons (join this interval) remaining)
      (cons interval acc))))

(defn row-hot-points
  "Find intervals of all positions along a line within
   the manhattan distances of x1,y1 from coord pairs [x1 y1 x2 y2]"
  [coords row]
  (->> (map #(manhattan-row-interval row %) coords)
       (filter some?)
       (sort-by first)
       (reduce merge-intervals [])))

(defn scan-row
  "Sum the valid intervals found above, removing beacon values
   laying on the given line (of course there's one)"
  [coords n]
  (let [row (int n)
        intervals (row-hot-points coords row)
        beacons-on-yval (->> (map #(subvec % 2) coords)
                             set
                             (filter #(= row (last %)))
                             (map first))
        gotchas (for [i intervals
                      b beacons-on-yval
                      :let [[s e] i]]
                  (<= s b e))]
    (->> (map #(inc (- (reduce - %))) intervals)
         (reduce + (- (count gotchas))))))

(comment
  ;; Number of invalid locations for beacons on y = 2,000,000
  (scan-row coords 2E6)
  ;; => 4907780

  ;; Find the first discontinuous interval along slices of the x-axis
  (->> (range 4E6 0 -1)
       (map (juxt identity #(row-hot-points coords %)))
       (some #(when (> (count (second %)) 1) %)))
  ;; => [2836448.0 ([3409991.0 4091997.0] [-551671.0 3409989.0])]

  ;; Add y-value to 4 million times x-value
  (+ 2836448 (* (int 4E6) 3409990))
  ;; => 13639962836448
  )
