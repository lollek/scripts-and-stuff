
; The cube's edges are named like this
; in neighbour-data
; 0-----1
; |\    |\
; | 4-----5
; 3-|---2 |
;  \|    \|
;   7-----6
(defn available-paths [data  current]
  "Returns the amount of neighbours which we have not yet visited"
  (let [neighbour-data [[1 3 4] [0 2 5] [1 3 6] [0 2 7]
                       [0 5 7] [1 4 6] [2 5 7] [3 4 6]]]
    (filter #(nil? (data %)) (neighbour-data current))))

(defn try-next [data current]
  (let [old-data-num (count(filter (complement nil?) data))
        new-data (assoc-in data [current] old-data-num)]
    (if (not-any? nil? new-data)
      1
      (reduce + (map #(try-next new-data %)
                     (available-paths new-data current))))))

(defn try-all []
  (let [data [nil nil nil nil nil nil nil nil]]
    (reduce + (map #(try-next data %) (range 0 8)))))

(println (try-all))
