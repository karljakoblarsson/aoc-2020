(ns aoc.day3
  (:require
   [clojure.repl]
   [clojure.pprint :as pp]
   [clojure.string :as s]
   [clojure.test :as t]
   ; [clojure.set :as st]
   ; [clojure.core.matrix :as m]
   ; [clojure.core.reducers :as r]
   ; [clojure.math.numeric-tower :as m]
   )
  (:use aoc.core))

(def day 3)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))

(defn parse-line [s]
  (map (fn [c] (= c \#)) s))

(defn prepare-input [str-input]
  (let [mat (map #(parse-line %) (s/split-lines str-input)) ]
    mat
    ))


(def t1 (prepare-input testfile))
(def test-input t1)
; (pp/pprint test-input)

(def input (prepare-input infile))

(defn trees [mat]
    (remove nil?
      (for [[y row] (map-indexed list mat)
            [x v] (map-indexed list row) ]
        (if v [x y] nil)))  )

(def t2 (trees t1))
; (pp/pprint (trees input))

; (t/are [i o] (= i o)
;        [1 1])

(def move [3 1])

(defn step [sx move oldpos]
  (let [[x' y'] (map + oldpos move)]
    [(mod x' sx) y'] ))

(defn steps [in move]
  (let [sy (count in)
        sx (count (first in))
        startpos [0 0]
        ]
    (reduce
      (fn [acc [x y]] (if (>= y sy) (reduced acc) (conj acc [x y]))
        )
      []
      (iterate #(step sx move %) startpos)
    )
  ))


; (pp/pprint (steps input))
; (pp/pprint (trees input))


(defn part1 [input]
  (let [trs (set (trees input))
        stps (steps input move)
        ]
    (count (filter trs stps))
    )
  )

(part1 t1)
(part1 input)
(println (time (part1 input)))

(def slopes
  [[1 1]
    [3 1]
    [5 1]
    [7 1]
    [1 2]])


(defn seen [input move]
  (let [trs (set (trees input))
        stps (steps input move)
        ]
    (count (filter trs stps))))


(defn part2 [input]
  (reduce *
          (map #(seen input %) slopes) ))

; (part2 t1)

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
