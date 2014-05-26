(defn fib [x]
  "Returns the nth fibonacci"
  ((fn [new-value old-value times]
     (if (> times 0)
       (recur (+ new-value old-value) new-value (dec times))
       new-value))
   0 1 x))

(defn test_fib []
  "Tests fib function"
  (println "Starting test-cases")
  (assert (= (fib 0) 0))
  (assert (= (fib 1) 1))
  (assert (= (fib 2) 1))
  (assert (= (fib 42) 267914296))
  (println "Success!"))

(if *command-line-args*
  (if (.equals (nth *command-line-args* 0) "test")
    (test_fib)
    (println (fib (read-string (nth *command-line-args* 0)))))
  (doseq [n (range 0 10)]
    (printf "%d\t%d\n" (fib n) (fib (+ n 10)))))

