(defn roman-to-decimal [word]
  "Turns a roman number into a decimal, e.g. XV to 15"
  ((fn [word result]
     (let [curr (first word)
           tail (rest word)]
       (case curr
         nil result
         \M (recur tail (+ result 1000))
         \D (recur tail (+ result 500))
         \C (case (first tail)
              \M (recur (rest tail) (+ result 900))
              \D (recur (rest tail) (+ result 400))
                 (recur       tail  (+ result 100)))
         \L (recur tail (+ result 50))
         \X (case (first tail)
              \C (recur (rest tail) (+ result 90))
              \L (recur (rest tail) (+ result 40))
                 (recur       tail  (+ result 10)))
         \V (recur tail (+ result 5))
         \I (case (first tail)
              \X (recur (rest tail) (+ result 9))
              \V (recur (rest tail) (+ result 4))
                 (recur       tail  (+ result 1)))
         (do
           (printf "Non-roman character received (%c)\n" curr)
           -1))))
   word 0))

(defn decimal-to-roman [word]
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


(defn is-digit? [num] (some #{num} "0123456789"))
(defn is-roman? [num] (some #{num} "MDCLXVI"))

(if *command-line-args*
  (doseq [i *command-line-args*]
    (cond
      (every? is-digit? i) (println (decimal-to-roman (read-string i)))
      (every? is-roman? i) (println (roman-to-decimal i))
      :else (println "Error: Unknown characters received")))
  (println "Example usage: ./progname XVI or ./progname 15"))
