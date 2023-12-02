(ns aoc.2022.week-2.day-12-b
  (:require [aoc.util :as util]
            [clojure.string :as s]))

(defonce input
  (util/get-input 2022 12))

(def height-map
  "2D array of number values (corresponds to ascii codes)."
  (let [start-end (-> input
                      (s/replace-first "E" "z")
                      (s/replace-first "S" "a"))]
    (->> start-end
         util/sl
         (mapv #(mapv int %)))))

(def start [20 120])
(def end [20 0])

(defn surrounding-points
  "Return valid neighbors UDLR of given coord."
  [coord w h]
  (let [[y x] coord]
    (cond-> []
      (< 0 x) (conj (update coord 1 dec))
      (< 0 y) (conj (update coord 0 dec))
      (< x (- w 2)) (conj (update coord 1 inc))
      (< y (- h 2)) (conj (update coord 0 inc)))))

(defn valid-targets
  [grid coord w h]
  (let [targets (surrounding-points coord w h)]
    (filter #(>= (inc (get-in grid %))
                 (get-in grid coord))
            targets)))

(defn all-targets
  [grid coords w h]
  (->> (map #(valid-targets grid % w h) coords)
       (reduce into #{})
       (remove coords)))

(defn explore
  [grid]
  (let [w (count (first grid))
        h (count grid)
        step (fn [[seen active]]
               (let [targets (all-targets grid active w h)]
                 [(into seen targets)
                  (set (remove active targets))]))]
    (->> (iterate step [#{start} #{start}])
         (map-indexed vector)
         (filter #((get-in % [1 1]) end))
         ffirst)))

(comment
  (->> height-map
       (map #(s/join (map char %)))
       (s/join \newline)
       (spit "assets/12back.txt"))
  (explore height-map))
