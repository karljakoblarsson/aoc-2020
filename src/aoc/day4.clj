(ns aoc.day4
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

(def day 4)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))

(defn parse-line [s]
  (map (fn [c] (= c \#)) s))

(defn parse-field [strs]
  (reduce
   (fn [acc f] (let [[_ kw v] (re-matches #"(.*)\:(.*)" f)] (assoc acc (keyword kw) v)))
   {}
   strs)
  )

(defn prepare-input [str-input]
  (let [records (partition-by-empty-line (s/split str-input #"\n"))
        fields (map (fn [r] (s/split (s/join " " r) #" ")) (vec records))
        ]
    ; (pp/pprint fields)
    (map parse-field  fields)
    ))

(prepare-input testfile)

(def t1 (prepare-input testfile))
(def test-input t1)
(def input (prepare-input infile))

(def required-fields
  [
    (keyword "byr")
    (keyword "iyr")
    (keyword "eyr")
    (keyword "hgt")
    (keyword "hcl")
    (keyword "ecl")
    (keyword "pid")
    ; (keyword "cid")
   ]
  )

(pp/pprint required-fields)

(defn check-pass [fields]
  (every? #(contains? fields %) required-fields))

(first t1)
(check-pass (first t1))



; (pp/pprint (steps input))
; (pp/pprint (trees input))


(map check-pass t1)

(defn part1 [input]
  (count-pred identity (map check-pass input))
  )

; (part1 t1)
; (part1 input)
; (println (time (part1 input)))


    ; byr (Birth Year) - four digits; at least 1920 and at most 2002.
    ; iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    ; eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    ; hgt (Height) - a number followed by either cm or in:
    ;     If cm, the number must be at least 150 and at most 193.
    ;     If in, the number must be at least 59 and at most 76.
    ; hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    ; ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    ; pid (Passport ID) - a nine-digit number, including leading zeroes.
    ; cid (Country ID) - ignored, missing or not.

(defn valid-pass [pass]
  (if (check-pass pass)
    (every? true? (vals
                   
  (let [{:keys [byr iyr eyr hgt hcl ecl pid]} pass]
    {
       :byr (let [y (read-string byr)] (and (>= y 1920) (<= y 2002)))
       :iyr (let [y (read-string iyr)] (and (>= y 2010) (<= y 2020)))
       :eyr (let [y (read-string eyr)] (and (>= y 2020) (<= y 2030)))
       :hgt (let [[_ d unit] (re-matches #"(\d+)(cm|in)" hgt)
             ; 0 since it will never match the range
             v (if (nil? d) 0 (read-string d))  ]
               (cond
                (= unit "in") (and (>= v 59) (<= v 76))
                (= unit "cm") (and (>= v 150) (<= v 193))
                 :else false))
        :hcl (some? (re-matches #"#[0123456789abcdef]{6}" hcl))
        :ecl (some? (#{ "amb" "blu" "brn" "gry" "grn" "hzl" "oth" } ecl)) 
        :pid (some? (re-matches #"\d{9}" pid))
     }
    ) 
                   ))
    false
    )
  )

(map valid-pass t1)
; (pp/pprint t1)

(valid-pass (first t1))
; (pp/pprint (first t1))


(defn part2 [input]
  (count-pred identity (map valid-pass input))
  )

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
