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

(def nums (-> datasets
              :puzzle1
              slurp
              (str/split #"\n")
              (#(map (fn [str] (Integer/parseInt str)) %))))

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
  (-> datasets
      :puzzle3
      slurp
      (str/split #"\n")))

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

;;; Puzzle 5 ;;;;;;;;;;;;;;;;;

(def forest 
  (-> datasets
      :puzzle5
      slurp
      (str/split #"\n")))

(def period (count (forest 1)))
(def depth (count forest))

(defn move [[x y] [dx dy]]
  (let [new-x (mod (+ x dx) period)
        new-y (+ y dy)]
    [new-x new-y]))

(defn tree? [[x y]]
  (= \# (get (forest y) x)))

(defn make-path [[start-x start-y] [dx dy]]
  (reverse 
   (reduce 
    (fn [path next-y] 
      (cons (move (first path) (list dx dy)) 
            path))
    [[start-x start-y ]] 
    (range start-y (- depth 1) dy))))

(defn count-trees [[dx dy]]
  (count-if tree? (make-path [0 0] [dx dy])))

(def slopes '((1 1) 
              (3 1)
              (5 1)
              (7 1)
              (1 2)))

(map count-trees slopes)
(reduce * 1 (map count-trees slopes))

