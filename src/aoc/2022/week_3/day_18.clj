(ns aoc.2022.week-3.day-18
  (:require [aoc.util :as util]))

(defonce input
  (util/get-input 2022 18))

(defonce sample-input
  (slurp "assets/18sample.txt"))

(def coords
  "Vector of [x y z] vectors."
  (->> input
       util/sl
       (mapv #(mapv util/str->int (re-seq #"\d+" %)))
       sort))

(def planes
  "plane access for (x, y), (x, z), (y, z)."
  [[[0 1] [0 2] [1 2]]
   [2 1 0]])

(defn group
  "Find each coordinate within a given slice proximally through a plane [m, n]."
  [plane coords]
  (let [[m n] plane]
    (group-by #(vector (% m) (% n)) coords)))

(defn consecutive-ranges
  "Given an ordered list of numbers, find the amount of discrete ranges."
  ([v] (consecutive-ranges 0 v))
  ([n v]
   (inc (reduce (fn [acc [m n]]
                  (if (= m (dec n)) acc (inc acc)))
                n
                (mapv vec (partition 2 1 v))))))

(defn find-surfaces
  "Given a slice of 3D coordinates along one axis, count its 'end caps.'"
  [index group]
  (-> group
      (update-vals (partial mapv #(% index)))
      (update-vals #(* 2 (consecutive-ranges %)))))

(defn surface-area
  "Find the interior and exterior surface area of a 3D voxel solid."
  [coord-set]
  (let [f (fn [coords plane index]
            (->> (group plane coords)
                 (find-surfaces index)
                 vals
                 (reduce +)))
        totals-by-plane (apply map #(f coord-set %1 %2) planes)]
    (reduce + totals-by-plane)))

(comment
  (surface-area coords))
