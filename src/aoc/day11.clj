(ns aoc.day10
  (:require
   [clojure.repl]
   [clojure.pprint :as pp]
   [clojure.string :as s]
   [clojure.test :as t]
   [clojure.set :as st]
   [clojure.data :as data]
   [clojure.walk :as w]
   [clojure.core.match :as mt]
   ; [clojure.core.matrix :as m]
   ; [clojure.core.reducers :as r]
   ; [clojure.math.numeric-tower :as m]
   )
  (:use aoc.core))

(def day 10)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))
(def testfile2 (slurp "inputs/test10b.txt"))


(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input)]
    (vec (map parse-int lines))))

(def t1 (prepare-input testfile))
(def t2 (prepare-input testfile2))
(def input (prepare-input infile))

(pp/pprint (sort t1))

(def outlet 0)
(defn device [input]
  (+ 3 (reduce max input)))

(defn lst [input]
  (sort (into input [outlet (device input)])))

(lst t1)

(defn diffs [input]
  (map - (rest input) input))

; (diffs (lst t1))

(defn part1 [input]
  (let [ds (diffs (lst input))
        freqs (frequencies ds)]
    (println (freqs 1) (freqs 3))
    (* (freqs 1) (freqs 3))
    ))

; (part1 t1)
; (part1 t2)
; (part1 input)
; (println (time (part1 input)))

; (println (-> t2 (lst) (diffs)))

(defn runs-of-one [input]
  (remove #(= 3 (first %))
    (partition-by
      (fn [a] a) (-> input (lst) (diffs)))))

; This line is key. No run of ones are longer than 4 in my input.
; So I don't need to calc the permutations for 5 or more.
; (reduce max (map count (runs-of-one input)))

(defn combs [n]
  ({
    1 1
    2 2
    3 4
    4 7
    } n))

(defn part2 [input]
  (reduce * (map combs (map count (runs-of-one input))))
    )

; (part2 t1)
; (part2 t2)
; (part2 input)

; (prn (time (part2 input)))
; (part2 input)

; 2 -> 1
; 3 -> 4
; 4 -> 6
; 5 -> 11

; 1 1 1 1 1
; 2 1 1 1
; 2 2 1
; 2 1 2
; 1 1 1 2
; 1 2 2
; 3 1 1
; 3 2
; 2 3
; 1 3 1
; 1 1 3


(defn solve-problem [infile]
  (let [input-string (slurp infile)
        input (prepare-input input-string)]
    (println "Part 1:")
    (println (time (part1 input)) )
    (println "")
    (println "Part 2:")
    (println (time (part2 input)) )))

(solve-problem (mk-input-filename day))
