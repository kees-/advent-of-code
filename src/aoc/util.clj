(ns aoc.util
  (:require [clojure.string]
            [clj-http.client :as http]
            [clojure.java.io :as io]))

(def sl "[s] Abbreviation for clojure.string/split-lines" clojure.string/split-lines)
(def s "[s re] [s re limit] Abbreviation for clojure.string/split" clojure.string/split)

;; ========== SITE-SPECIFIC ====================================================
;; session=...
(def cookie
  (slurp "assets/token"))

(defn get-input
  "Digit 1-25 retrieves daily AOC input."
  [year day]
  (let [file (format "assets/%s/%02d.txt" year day)
        url (format "https://adventofcode.com/%s/day/%s/input" year day)]
    (when-not (.exists (io/file file))
      (println "Attempting to grab input from adventofcode.com...")
      (->> (http/get url
                     {:headers {"Cookie" cookie}})
           :body
           (spit file)))
    (slurp file)))

;; ========== GENERAL PURPOSE FNS ==============================================
;; Integer/parseInt doesn't map anonymously on sequences well
(defn str->int
  "Coerce string to integer"
  [s]
  (Integer/parseInt s))

;; https://groups.google.com/g/clojure-dev/c/NaAuBz6SpkY
(defn take-until
  "Returns a lazy sequence of successive items from coll until
   (pred item) returns true, including that item. pred must be
   free of side-effects."
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (if (pred (first s))
       (cons (first s) nil)
       (cons (first s) (take-until pred (rest s)))))))

(defn pairs->map
  "Given a list of pairs, regroup into a map of values to list of keys.
   fs are 1-arity collection operations that subsequently map onto the vals."
  [coll & fs]
  (let [operations (reverse (into [#(map first %)] fs))
        f (apply comp operations)]
    (-> (group-by second coll)
        (update-vals f))))
