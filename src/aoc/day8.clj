(ns aoc.day8
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

(def day 8)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))

; (parse-line (first (s/split-lines testfile)))


(defn parse-line [line]
  (let [matches (re-matches #"^(.+) ([+-]\d+)$" line)
        [_ instr arg] matches
        ]
    { :inst (keyword instr) :arg (parse-int arg)}))


(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input)]
    (into [] (map parse-line lines))))

(def t1 (prepare-input testfile))
(def input (prepare-input infile))

(pp/pprint t1)

(def init-state { :line 1 :acc 0 :visited #{} })

(defn exec-inst [{:keys [line acc visited]} {:keys [inst arg]}]
  ; (println line acc visited)
  (mt/match [inst arg]
    [:nop _] { :line (inc line) :acc acc :visited (conj visited line)}
    [:acc n] { :line (inc line) :acc (+ acc n) :visited (conj visited line)}
    [:jmp offset] { :line (+ line offset) :acc acc :visited (conj visited line)}
    )
  )

(defn exec-line [state lines]
  (exec-inst state (nth lines (dec (:line state)) )))

; (exec-line init-state t1)

(defn is-visited [{ :keys [line visited] :as state }]
  (if (visited line)
    state
    nil
    ))

(defn exec [lines]
  (some is-visited (iterate #(exec-line % lines) init-state))
  )

; (exec t1)

(defn part1 [input]
  (:acc (exec input)
    ))

; (part1 t1)
; (part1 input)
; (println (time (part1 input)))

(defn last-line [lines] (inc (count lines)))

(defn has-halted [{ :keys [line]} last-line]
  (if (= line last-line)
    true
    nil))

(defn halt-or-not [{ :keys [line visited] :as state } last-line]
  (cond
    (has-halted state last-line) :halt
    (visited line) :loop
    :default nil
    ))

(defn exec-from [lines line-no]
  (case (some
       #(halt-or-not % (last-line lines))
       (iterate #(exec-line % lines) (assoc init-state :line line-no)))
    :halt line-no
    :loop nil
   ))

(defn halting-line-nos [lines]
  (let [line-res (map-indexed (fn [k _] (exec-from lines (inc k))) lines)
        ]
    (set (remove nil? line-res))
   ))

; (halting-line-nos input)
; (exec-from input 621)
(defn nop-would-halt [line offset halting-lines]
  (halting-lines (+ line offset)))

(defn jmp-would-halt [line halting-lines]
  (halting-lines (inc line)))

(defn should-change-to [{:keys [line acc visited]} lines halting-lines]
  (let [{ :keys [inst arg] } (nth lines (dec line))]
    (mt/match [inst arg]
      [:acc n] { :line (inc line) :acc (+ acc n) :visited (conj visited line)}
      [:nop offset] (if (nop-would-halt line offset halting-lines)
          (reduced { :line-no line :to :jmp })
          { :line (inc line) :acc acc :visited (conj visited line)}
                      )
      [:jmp offset] (if (jmp-would-halt line halting-lines)
          (reduced { :line-no line :to :nop })
          { :line (+ line offset) :acc acc :visited (conj visited line)}
                      ))))

; (defn exec-line-change [state lines]
  ; (exec-inst-change state lines halting-lines))

(defn find-change [lines]
  (let [halting-lines (halting-line-nos lines)]
    (reduce (fn [acc _] (should-change-to acc lines halting-lines)) init-state (repeat 1))
    ))

(find-change input)

(defn apply-change [lines]
  (let [{ :keys [line-no to]} (find-change lines)]
    (update lines (dec line-no) #(assoc % :inst to))
    ))

(defn is-end [{:keys [line] :as state} lines]
  (if (= (last-line lines) line)
    state
    nil
   ))

(defn exec-to-halt [lines]
  (some #(is-end % lines) (iterate #(exec-line % lines) init-state))
  )

(defn part2 [input]
  (let [changed (apply-change input) ]
    (:acc (exec-to-halt changed))
    )
  )

(part2 input)


; (defn is-jump-to-last [{:keys [line inst arg]} lasts]
;   (print line lasts)
;   (let []
;     (mt/match [inst arg]
;       [:nop _] (some (conj #{} (inc line)) lasts )
;       [:acc n] (some (conj #{} (inc line))  lasts)
;       [:jmp offset] (some (conj #{} (+ line offset))  lasts)
;       )))

; (defn terminating-lines [lines]
;  (let [num-lines (map-indexed (fn [i v] (assoc v :line (inc i))) lines)
;        rev (reverse num-lines)
;        end-line (last-line lines)]
;     (println (first rev ) (is-jump-to-last (first rev ) [end-line]))
;     (vec (take-while #(is-jump-to-last % [end-line]) rev))
;     ))

; (terminating-lines t1)

(defn part2 [input]
  ; Don't include the end bag itself in the count
  (dec (get-end input)))

; (part2 t1)
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
