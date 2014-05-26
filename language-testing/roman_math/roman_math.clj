
(defn digit? [num] (some #{num} "0123456789"))
(defn roman? [num] (some #{num} "MDCLXVI"))

(defn roman->decimal [roman-word]
  "Turns a roman number into a decimal, e.g. XV to 15"
  (let [->number {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}
        negative-values {\C "MD" \X "CL" \I "XV"}]
    ((fn [word result]
       (if-let [this (roman? (first word))]
         (if-let [next- (some #{(first (rest word))} (negative-values this))]
           (recur (drop 2 word) (+ result (- (->number next-) (->number this))))
           (recur (drop 1 word) (+ result (->number this))))
         result))
     roman-word 0)))

(defn decimal->roman [word]
  "Turns a decimal number into roman, e.g. 15 to XV"
  ((fn [number result]
     (condp <= number
       1000 (recur (- number 1000) (str result \M))
       900  (recur (- number 900)  (str result \C \M))
       500  (recur (- number 500)  (str result \D))
       400  (recur (- number 400)  (str result \C \D))
       100  (recur (- number 100)  (str result \C))
       90   (recur (- number  90)  (str result \X \C))
       50   (recur (- number  50)  (str result \L))
       40   (recur (- number  40)  (str result \X \L))
       10   (recur (- number  10)  (str result \X))
       9    (recur (- number   9)  (str result \I \X))
       5    (recur (- number   5)  (str result \V))
       4    (recur (- number   4)  (str result \I \V))
       1    (recur (- number   1)  (str result \I))
       result))
   word ""))



(if *command-line-args*
  (doseq [i *command-line-args*]
    (cond
      (every? digit? i) (println (decimal->roman (read-string i)))
      (every? roman? i) (println (roman->decimal i))
      :else (println "Error: Unknown characters received")))
  (println "Example usage: ./progname XVI or ./progname 15"))
