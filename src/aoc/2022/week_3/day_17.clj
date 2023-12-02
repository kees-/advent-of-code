;; < 2990 answer 3277

(ns aoc.2022.week-3.day-17
  (:require [aoc.util :as util]))

(defonce input
  (util/get-input 2022 17))

(defonce sample
  (slurp "assets/17sample.txt"))

(def instructions
  (->> (util/s input #"")
       (mapv #(if (= % "<") :l :r))
       (interpose :d)
       cycle))

(def blocks
  (let [normalize (fn [row]
                    (vec (take 7 (into
                                  (into [0 0] row)
                                  (repeat 4 0)))))]
    (->> [[[1 1 1 1]]
          [[0 1 0] [1 1 1] [0 1 0]]
          [[0 0 1] [0 0 1] [1 1 1]]
          [[1] [1] [1] [1]]
          [[1 1] [1 1]]]
         (mapv #(mapv normalize %))
         cycle
         (take 2022))))

(defn one? [n] (= 1 n))

(defn insert
  "Adds one block to the top of the stack, 3 blocks above the highest point."
  [stack block]
  (reduce into [[0]] [block
                      (repeat 3 [0 0 0 0 0 0 0])
                      stack]))

(defn move-to
  [coord dir]
  (case dir
    :l (update coord 1 dec)
    :r (update coord 1 inc)
    :d (update coord 0 inc)))

(defn valid-move?
  [stack coord dir]
  (let [to (move-to coord dir)
        go-to (get-in stack to)]
    (and (<= 0 (to 1) 6)
         (some? go-to)
         (not= 2 go-to))))

(defn move
  "Apply one instruction to shift the active block one space in specified dir."
  [stack dir]
  (let [[above active below] (mapv vec (partition-by #(some one? %) stack))
        stratum (conj active (first below))
        moves (for [x (range 7)
                    y (range (count active))
                    :let [coord [y x]]
                    :when (one? (get-in stratum coord))]
                coord)]
    (if (some false? (map #(valid-move? stratum % dir) moves))
      stack
      (let [clear (reduce #(assoc-in %1 %2 0) stratum moves)
            applied (reduce #(assoc-in %1 %2 1) clear (map #(move-to % dir) moves))]
        (reduce into above [applied (next below)])))))

(defn set-block
  "Deactivates all block components in the given stack."
  [stack]
  (mapv (fn [row]
          (mapv #(if (one? %) 2 %)
                row))
        stack))

(defn drop-block
  "Reducer that applies"
  [[initial instructions] block]
  (loop [stack (insert initial block)
         [dir & queue] instructions]
    (let [trial (move stack dir)]
      (if (and (= dir :d)
               (= stack trial))
        (let [set-stack (->> stack
                             (drop-while #(every? zero? %))
                             set-block)]
          [set-stack queue])
        (recur trial queue)))))

(defn drop-blocks
  [instructions blocks]
  (first (reduce drop-block [nil instructions] blocks)))

(comment
  (->> (drop-blocks instructions blocks)
       count
       dec))
