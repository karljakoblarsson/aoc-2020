(ns aoc.day6
  (:require
   [clojure.repl]
   [clojure.pprint :as pp]
   [clojure.string :as s]
   [clojure.test :as t]
   [clojure.set :as st]
   [clojure.data :as data]
   ; [clojure.core.matrix :as m]
   ; [clojure.core.reducers :as r]
   ; [clojure.math.numeric-tower :as m]
   )
  (:use aoc.core))

(def day 6)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))


(defn prepare-input [str-input]
  (let [lines (map seq (s/split-lines str-input)) 
        groups (partition-by-empty-line lines) ]
    groups))

(def t1 (prepare-input testfile))
(def input (prepare-input infile))

(println t1)

(def t2 (first input))

(defn part1 [input]
  (sum (map (fn [in] (count (reduce (fn [acc item] (conj acc item)) #{} in)))  (map flatten input))))

; (part1 t1)
; (part1 input)
; (println (time (part1 input)))


(println (first input))

(defn part2 [input]
  (sum (map (fn [in] (count (reduce (fn [acc item] (st/intersection acc item)) (map set in))))  input)))

; (part2 input)

; (prn (time (part2 input)))
; (part2 input)

(defn solve-problem [infile]
  (let [input-string (slurp infile)
        input (prepare-input input-string)]
    (println "Part 1:")
    (println (time (part1 input)) )
    (println "")
    (println "Part 2:")
    (println (time (part2 input)) )))

(solve-problem (mk-input-filename day))
