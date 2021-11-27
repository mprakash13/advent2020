;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://adventofcode.com/2020
;;;;;;;;;;;;;;;;;;;;;;;;;;;


(ns fpgs-intro.advent2020)
(require '[clojure.core])
(require '[clojure.string :as str])
(require '[clojure.java.io :as io])
; (require '(clojure.java.integer :as int))

(defn puz1 [nums]
  (let [pairs (for [x nums
                    y nums
                    :when (< x y)] (list x y))
        pair (filter (fn [x] (= 2020 (apply + x))) pairs)]
    (apply * (first pair))))

(defn puz2 [nums]
  (let [trips (for [x nums
                    y nums
                    z nums
                    :when (and (< x y) (< y z))] (list x y z))
        trip (filter #(= 2020 (apply + %)) trips)]
    (apply * (first trip))))


(def nums-str  (slurp "resources/puz1-2.txt"))

(def nums (map #(Integer/parseInt %) (str/split nums-str #"\n")))


;;; Puzzle 3 ;;;;;;;;;;

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

(defn count-chars [kar strng]
  (reduce (fn [acc ltr] (if (= ltr kar) (inc acc) acc))
          0 strng))

(defn valid-pwd [lo hi kar pwd]
  (let [cnt (count-chars kar pwd)]
    (and (<= lo cnt) (>= hi cnt))))

(defn count-if [pred seq]
  (reduce (fn [acc elem] (if (pred elem) (inc acc) acc)) seq))

(def pwd-file 
  (str/split (slurp "resources/puz3-4.txt") #"\n"))
   
 
;
;  Puzzle 4
;

(defn valid-pwd4 [lo hi kar pwd]
  (let [lo-match (= (get pwd (- lo 1)) kar)
         hi-match (= (get pwd (- hi 1)) kar)]
     (not= lo-match hi-match)))

(defn count-valid4 [pwd-file]
  (count (filter #(let [[lo hi kar pwd] (parse-pwd %)]
           (valid-pwd4 lo hi kar pwd))
       (filter #(not= "" %) pwd-file) )))