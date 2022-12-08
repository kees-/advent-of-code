(ns aoc.week-2.day-08
  (:require [aoc.util :as util]))

(defonce input
  (util/get-input 8))

(def rows
  (->> input
       util/sl
       (mapv (comp
              #(mapv util/str->int %)
              #(util/s % #"")))))

(def rows-rotated
  (apply mapv vector rows))

(def width (count (first rows)))
(def height (count rows))

(comment
  (->> (for [x (range width)
             y (range height)]
         (let [row (rows y)
               r-row (rows-rotated x)
               blocked? #(some (partial <= (row x)) %)
               slices [(subvec row 0 x)
                       (subvec r-row 0 y)
                       (subvec row (inc x))
                       (subvec r-row (inc y))]
               [before r-before after r-after] (mapv blocked? slices)]
           (cond-> 0
             before inc
             after inc
             r-before inc
             r-after inc)))
       (remove #(= 4 %))
       count)
  ;; => 1796

  (->> (for [x (range width)
             y (range height)
             :let [row (rows y)
                   r-row (rows-rotated x)
                   slices [(reverse (subvec row 0 x))
                           (reverse (subvec r-row 0 y))
                           (subvec row (inc x))
                           (subvec r-row (inc y))]
                   search (fn [c]
                            (->> c
                                 (util/take-until #(<= (row x) %))
                                 count))]]
         (map search slices))
       (map #(reduce * %))
       (apply max))
  ;; => 288120
  )
