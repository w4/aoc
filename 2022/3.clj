#!/usr/bin/env nix-shell
#!nix-shell --pure -i bb -p babashka

(require '[clojure.java.io :as io])
(require '[clojure.set :as set])

; a-z -> 1->26, A-Z -> 27->52
(defn char-to-number [char]
  (cond
    (Character/isLowerCase char) (- (int char) 96)
    (Character/isUpperCase char) (- (int char) 38)))

; splits a string into two sets of characters
(defn split-half [s]
  (let [idx (quot (count s) 2)
        [left right] (split-at idx s)]
    [(set (apply str left)) (set (apply str right))]))

; splits a string in half, stores it in a set and finds the intersection
(defn part1-process-line [line]
  (->> (split-half line)
       (apply set/intersection)))

; processes each line and figures out the "priority" of the intersection
(defn part1 [lines]
  (->> lines
       (map part1-process-line)
       (mapcat identity)
       (map char-to-number)
       (reduce +)))

; processes each line and figures out which characters are shared between
; 3 lines and sums their "priority"
(defn part2-process-lines [group]
  (let [[g1 g2 g3] group]
    (->> (set/intersection (set g1) (set g2) (set g3))
         (map char-to-number)
         (reduce +))))

; splits the input up into 3 lines, processes the "priority" and sums
(defn part2 [lines]
  (->> (partition 3 lines)
       (map part2-process-lines)
       (reduce +)))

; read the entirety of stdin
(def lines (with-open [rdr (io/reader *in*)] (vec (->> (line-seq rdr)))))

(println (part1 lines))
(println (part2 lines))