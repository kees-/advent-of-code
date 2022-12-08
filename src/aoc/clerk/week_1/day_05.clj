; # Supply Stacks

(ns aoc.clerk.week-1.day-05
  (:require [aoc.util :as util]
            [clojure.string :as s]
            [nextjournal.clerk :as clerk]))

(def input
  (util/get-input 5))

; Read from a file `assets/05.txt`, requesting and writing from adventofcode.com if necessary.

;; ## Parsing input

;; Each step would be bound locally if the target weren't clerk.

(def crates-str
  (-> input
      (s/split #"\n\n") first  ;; Pull the crate stacks from str
      s/split-lines
      butlast))                ;; Remove the axis labels

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(def space? (partial = " "))

;; Becomes...

(->> crates-str
     (map #(re-seq #"\w" %))     ;; Isolate the letters
     (apply map list)            ;; Rotate the array
     (mapv #(remove space? %)))  ;; Drop the excess spaces

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(def crates
  (->> crates-str (map #(take-nth 4 (rest %)))
       (apply map list)
       (mapv #(remove (partial = \space) %))))

(def instructions-str
  (-> input
      (s/split #"\n\n") last  ;; Pull the instruction list from str
      s/split-lines))

;; Becomes...

(mapv (comp #(update % 2 dec)        ;; 3) dec the to and from array indices
            #(update % 1 dec)
            #(mapv util/str->int %)  ;; 2) Coerce into numbers
            #(re-seq #"\d+" %))      ;; 1) Split to tuples of [n from to] 
      instructions-str)

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(def instructions
  (mapv (comp #(update % 2 dec)        ;; 3) dec the to and from array indices
              #(update % 1 dec)
              #(mapv util/str->int %)  ;; 2) Coerce into numbers
              #(re-seq #"\d+" %))      ;; 1) Split to tuples of [n from to] 
        instructions-str))

;; ---

(defn rearrange-crates
  [direction]
  (fn [crates [n from to]]
    (let [to-move (->> (crates from) (take n) direction)]
      (-> crates
          (update from #(drop n %))
          (update to #(reduce conj % to-move))))))

;; Closure to apply one instruction for either part of the challenge.

;; Providing `identity` or `reverse` makes the crane reverse or maintain the `n` elements it `conj`es to the list at index `to`.

crates

(first instructions)

((rearrange-crates identity)
 crates
 (first instructions))

;; ## Part 1: crane moves incrementally

(->> instructions
     (reduce (rearrange-crates identity) crates)
     (map first)
     (reduce str))

;; ## Part 2: crane moves as bundle

(->> instructions
     (reduce (rearrange-crates reverse) crates)
     (map first)
     (reduce str))

;; ---

;; Combined without extra bindings:

^{:nextjournal.clerk/visibility {:code :fold :result :hide}}
(let [[crates instructions] (map s/split-lines
                                 (s/split input #"\n\n"))
      crates (->> (butlast crates)
                  (map #(take-nth 4 (rest %)))
                  (apply map list)
                  (mapv #(remove (partial = \space) %)))
      instructions (mapv (comp #(update % 2 dec)
                               #(update % 1 dec)
                               #(mapv util/str->int %)
                               #(re-seq #"\d+" %))
                         instructions)]
  (->> instructions
       (reduce (rearrange-crates identity) crates)
       (map first)
       (reduce str)))
