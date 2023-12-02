(ns aoc.2022.week-3.day-20-b
  (:require [aoc.util :as util]
            [clojure.core.matrix :as m]))

(defonce input
  (util/get-input 2022 20))

(def sample-input (slurp "assets/20sample.txt"))

(def encrypted
  (mapv #(vector %1 (util/str->int %2))
        (range)
        (util/sl input)))

(defn normalize
  [n length multiplier]
  (case (compare n 0)
    -1 (update (some #(when (<= 0 (second %)) %)
                     (iterate #(vector (dec (% 0))
                                       (+' (% 1) length))
                              [1 n]))
               1 dec)
    1 (some #(when (< (second %) length) %)
            (iterate #(vector (inc (% 0))
                              (-' (% 1) length))
                     [0 n]))
    [0 0]))

(defn n-index
  "Find the real index of tuple whose first value (original index) is i."
  [coll i]
  (first (keep-indexed #(when (= i (first %2)) %1) coll)))

(defn v-index
  "Find the real index of tuple whose first value (original index) is i."
  [i coll]
  (first (keep-indexed #(when (= i (second %2)) %1) coll)))

(defn to-front
  [i coll]
  (m/rotate coll 0 i))

(defn shift
  [coll n multiplier]
  (let [i (n-index coll n)
        v (reduce + (normalize (second (coll i))
                               (count coll)
                               multiplier))
        shifted (to-front i coll)
        before (subvec shifted 1 (inc v))
        after (subvec shifted (inc v))
        reconstructed (reduce into [before [(first shifted)] after])]
    reconstructed))

(defn decrypt
  [multiplier encrypted]
  (println "decrypting...")
  (reduce #(shift %1 %2 multiplier)
          encrypted
          (range (count encrypted))))

(defn extract-coords
  [decrypted]
  (->> decrypted
       (to-front (v-index 0 decrypted))
       cycle
       (take-nth 1000)
       (take 4)))

(defn finalize
  [coll]
  (reduce + (map second coll)))

;; 1769264353540 < answer

(comment
  ;; Sum of the necessary 'coordinates' in the decrypted number cycle
  (->> encrypted
       (decrypt 1)
       extract-coords
       finalize)
  ;; => 15297

  (->> encrypted
       (decrypt 811589153)
       extract-coords
       #_finalize)
  (->> encrypted
       (iterate #(decrypt 811589153 %))
       (#(nth % 9))
       extract-coords
       #_finalize))
