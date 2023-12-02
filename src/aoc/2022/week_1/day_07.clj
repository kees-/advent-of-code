(ns aoc.2022.week-1.day-07
  (:require [aoc.util :as util]))

;; Raw string input
(defonce input
  (util/get-input 2022 7))

;; Since ls is the only shell command with output, no need to be parsed
;; cd tries all dirs so parsing "dir ..." lines is redundant
;; Outputs a list like this: [["$" "cd" "/"] ["16394" "tllvcdr.sjl"] ["195891" "zbdp.gqb"] ... ]
(def history
  (->> input
       util/sl
       (remove #(re-find #"(\$ ls)|(^dir)" %))
       (mapv #(util/s % #" "))))
;; All remaining lines look like one of these:
;; ["$" "cd" ".."]
;; ["$" "cd" DIRNAME]
;; [SIZE FILENAME]
;; Root ("/") can be treated as any other named dir.

(defn add-sizes
  "Given a flat map of path vectors, add size value to all kvs that represent parents of the current path."
  [m path [s]]
  (loop [z m
         p path]
    (if (seq p)
      (recur
       (update z p + (util/str->int s))
       (subvec p 0 (dec (count p))))
      z)))
;; For example: (add-sizes
;;               {["/"] 50
;;                ["/" "a"] 50
;;                ["/" "b"] 0
;;                ["/" "b" "c"] 0}
;;               ["/" "b" "c"]
;;               100)
;; Does this:
;; (update h ["/" "b" "c"] + 100)
;; (update h ["/" "b"] + 100)
;; (update h ["/"] + 100)
;; => {["/"] 150
;;     ["/" "a"] 50
;;     ["/" "b"] 100
;;     ["/" "b" "c"] 100}

(defn parse-step
  "Distinguishes the given command this and transforms map as necesssary."
  [m path this]
  (let [[a _ c] this]
    (cond
      ;; Map doesn't change if the command is going up one directory:
      (= ".." c) m

      ;; 'Creates' a new directory by associating [[path] dir]:
      (= "$" a) (assoc m (conj path c) 0)

      ;; All cases are addressed except for file sizes.
      ;; Since files weight all parents, multiple kvs need to be updated:
      :else (add-sizes m path this))))

;; The success of this function leans on the given data being consistent.
;; AOC author lists all directories in a depth-first search.
;; Checks are not necessary because locations are not revisited.

(defn update-path
  "'$ cd _' is the only command with three elements. If a third arg is present, remove final element from path if upward and add new element if downward."
  [path [_ _ d]]
  (cond
    (= ".." d) (subvec path 0 (dec (count path)))
    d (conj path d)
    :else path))

(defn parse
  "Discovers a virtual file system by iterating each command or result."
  [history]
  (loop [filesystem {}
         path []
         [current & remaining] history]
    (if current
      (recur
       (parse-step filesystem path current)
       (update-path path current)
       remaining)
      filesystem)))

(comment
  ;; Sum all directories below 100kb in size
  (->> (parse history)
       (map last)
       (filter #(<= % 1E5))
       (reduce +))
  ;; => 1642503

  ;; Find the smallest directory causing storage to exceed 40mb
  (let [h (parse history)
        space-needed (- (get h ["/"]) 4E7)]
    (->> (map last h)
         sort
         (some #(when (< space-needed %) %))))
  ;; => 6999588
  )
