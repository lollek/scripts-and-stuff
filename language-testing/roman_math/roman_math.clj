(defn roman-to-decimal [word]
  "Turns a roman number into a decimal, e.g. XV to 15"
  ((fn [word result]
     (let [curr (first word)
           tail (rest word)]
       (cond
         (nil? curr) result
         (= curr \M) (recur tail (+ result 1000))
         (= curr \D) (recur tail (+ result 500))
         (= curr \C)
         (cond
           (= (first tail) \M) (recur (rest tail) (+ result 900))
           (= (first tail) \D) (recur (rest tail) (+ result 400))
           :else               (recur       tail  (+ result 100)))
         (= curr \L) (recur tail (+ result 50))
         (= curr \X)
         (cond
           (= (first tail) \C) (recur (rest tail) (+ result 90))
           (= (first tail) \L) (recur (rest tail) (+ result 40))
           :else               (recur       tail  (+ result 10)))
         (= curr \V) (recur tail (+ result 5))
         (= curr \I)
         (cond
           (= (first tail) \X) (recur (rest tail) (+ result 9))
           (= (first tail) \V) (recur (rest tail) (+ result 4))
           :else               (recur       tail  (+ result 1)))
         :else (do
                 (printf "Non-roman character received (%c)\n" curr)
                 -1))))
   word 0))

(defn decimal-to-roman [word]
  "Turns a decimal number into roman, e.g. 15 to XV"
  ((fn [number result]
     (cond
       (>= number 1000) (recur (- number 1000) (str result \M))
       (>= number 900)  (recur (- number 900)  (str result \C \M))
       (>= number 500)  (recur (- number 500)  (str result \D))
       (>= number 400)  (recur (- number 400)  (str result \C \D))
       (>= number 100)  (recur (- number 100)  (str result \C))
       (>= number  90)  (recur (- number  90)  (str result \X \C))
       (>= number  50)  (recur (- number  50)  (str result \L))
       (>= number  40)  (recur (- number  40)  (str result \X \L))
       (>= number  10)  (recur (- number  10)  (str result \X))
       (>= number   9)  (recur (- number   9)  (str result \I \X))
       (>= number   5)  (recur (- number   5)  (str result \V))
       (>= number   4)  (recur (- number   4)  (str result \I \V))
       (>= number   1)  (recur (- number   1)  (str result \I))
       :else result))
   word ""))


(defn is-digit? [num] (some #(= num %) "0123456789"))
(defn is-roman? [num] (some #(= num %) "MDCLXVI"))

(if *command-line-args*
  (doseq [i *command-line-args*]
    (cond
      (every? is-digit? i) (println (decimal-to-roman (read-string i)))
      (every? is-roman? i) (println (roman-to-decimal i))
      :else (println "Error: Unknown characters received")))
  (println "Example usage: ./progname XVI or ./progname 15"))
