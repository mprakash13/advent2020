;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://adventofcode.com/2020
;;;;;;;;;;;;;;;;;;;;;;;;;;;


(ns fpgs-intro.advent2020)
(require '[clojure.core])
(require '[clojure.string :as str])
(require '[clojure.java.io :as io])

(def datasets {:puzzle1 "resources/data1.txt"
               :puzzle2 "resources/data1.txt"
               :puzzle3 "resources/data2.txt"
               :puzzle4 "resources/data2.txt"
               :puzzle5 "resources/data3.txt"})

;;;;;;;;;;;;;;;;;;;;;
;; Puzzles 1 & 2
;;;;;;;;;;;;;;;;;;;;;


(def nums-str  (slurp (:puzzle1 datasets)))

(def nums (map #(Integer/parseInt %) (str/split nums-str #"\n")))

(defn puz1 [nums]
  (for [x nums
        y nums
        :when (and (< x y) (= 2020 (+ x y)))]
    (* x y)))

(defn puz2 [nums]
  (for [x nums
        y nums
        z nums
        :when (and (< x y) (< y z) (= 2020 (+ x y z)))]
    (* x y z)))

;;; Puzzle 3 & 4 ;;;;;;;;;;

(def pwd-file
  (str/split (slurp (:puzzle3 datasets)) #"\n"))

(defn parse-pwd [line]
  (let [sane-str
        (str/replace line
                     #"([0-9]+)-([0-9]+)[ ]+([a-z]):[ ]+([a-z]+)"
                     "$1-$2-$3-$4")
        [min-str max-str kar-str pwd] (str/split sane-str #"-")
        lo (Integer/parseInt min-str)
        hi (Integer/parseInt max-str)
        kar (first kar-str)]
    (list lo hi kar pwd)))

(defn count-if [pred seq]
  (reduce (fn [acc elem] (if (pred elem) (inc acc) acc)) 0 seq))

(defn count-chars [kar strng]
  (count-if #(= kar %) strng))
;  (reduce (fn [acc ltr] (if (= ltr kar) (inc acc) acc))
;          0 strng))

(defn valid-pwd3 [lo hi kar pwd]
  (let [cnt (count-chars kar pwd)]
    (and (<= lo cnt) (>= hi cnt))))

(defn count-valid3 [pwd-file]
  (count-if #(apply valid-pwd3 %)
            (map parse-pwd pwd-file)))


(defn valid-pwd4 [lo hi kar pwd]
  (let [lo-match (= (get pwd (- lo 1)) kar)
        hi-match (= (get pwd (- hi 1)) kar)]
    (not= lo-match hi-match)))

(defn count-valid4 [pwd-file]
   (count-if #(apply valid-pwd4 %)
            (map parse-pwd pwd-file)))
