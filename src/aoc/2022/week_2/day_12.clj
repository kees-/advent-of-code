(ns aoc.2022.week-2.day-12
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

height-map

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

(defn update-visited
  "Return a reducer that updates all path lengths surrounding one coord."
  [grid w h]
  (fn [paths coord]
    (let [tests (surrounding-points coord w h)
          traversal #(cond-> %1
                       (and (not (get-in %1 %2))
                            (get-in paths coord)
                            (>= (inc (get-in grid %2))
                                (get-in grid coord)))
                       (assoc-in %2 (inc (get-in paths coord))))]
      (reduce traversal paths tests))))

(defn next-queue
  "Iterate queue by finding all unique coords adjacent to current coord set."
  [queue w h]
  (reduce into #{} (map #(surrounding-points % w h) queue)))

(defn search
  "Populate a grid with the path length to any coord until `end` is found."
  [grid]
  (let [w (count (first grid))
        h (count grid)]
    (loop [paths (assoc-in (->> (repeat w nil) vec (repeat h) vec) start 0)
           queue #{start}]
      (if (get-in paths end)
        paths
        (recur (reduce (update-visited grid w h) paths queue)
               (next-queue queue w h))))))

(comment
  (get-in (search height-map) end)

  (->> height-map
       search
       (map #(map (partial format "% 4d") %))
       (map #(s/join \space %))
       (s/join \newline)
       (spit "assets/12out.txt")))
