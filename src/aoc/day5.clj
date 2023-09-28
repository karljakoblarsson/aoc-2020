(ns aoc.day5
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

(def day 5)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))

(defn parse-line [s]
  (map (comp keyword str) s))

(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input) ]
    (map parse-line lines)))

(def t1 (prepare-input testfile))
(def test-input t1)
(def input (prepare-input infile))

(def rows 127)
(def columns 7)

(def t2 (first t1))

(defn split-rc [in]
  (let [[row col] (partition-by (fn [e] (contains? #{:F :B} e)) in)]
    { :row row :col col}))

(split-rc t2)

(def t3 (:row (split-rc t2)))

(defn calc-row [lst [l h]]
  (let [half (quot (- h l) 2)]
    (cond
      (empty? lst) l
      (contains? #{:F :L} (first lst)) (recur (rest lst) [l (- h half 1)])
      (contains? #{:B :R} (first lst)) (recur (rest lst) [(+ l half 1) h])
    )))

(defn calc-rc [in]
  (let [{ :keys [row col]} (split-rc in)]
    { :row (calc-row row [0 rows])
      :column (calc-row col [0 columns]) }))

; (calc-row t3 [0 rows])
; (calc-rc t2)
; (:col (split-rc t2)) 


(t/are [i o] (= (calc-rc (parse-line i)) o)
  "BFFFBBFRRR" { :row 70 :column 7 }
  "FFFBBBFRRR" { :row 14 :column 7 }
  "BBFFBBFRLL" { :row 102 :column 4 })

(defn seatId [in]
  (let [{ :keys [row column]} (calc-rc in)]
    (+ (* row 8) column)
    ))

(t/are [i o] (= o (seatId (parse-line i)))
  "BFFFBBFRRR" 567
  "FFFBBBFRRR" 119
  "BBFFBBFRLL" 820)


(defn part1 [input]
  (reduce max (map seatId input)))

(part1 t1)
(part1 input)
; (println (time (part1 input)))


(def ids (map seatId input))

(st/difference (set [1 2 3 4]) (set [1 2])  )

(defn part2 [input]
  (let [ids (map seatId input)
        mx (reduce max ids)
        mi (reduce min ids)]
    (first (st/difference (set (range mi (inc mx))) (set ids)))
    ))

(defn part2B [input]
  (reduce (fn [acc n] (if (> (- n acc) 1) (reduced (dec n)) n))
          (sort (map seatId input)) ))

(part2 input)

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
