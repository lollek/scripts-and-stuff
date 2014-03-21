(defn fib [x]
  "Returns the nth fibonacci"
  (defn fib_ [new_value old_value times]
    "Tail recursive fibonacci"
    (if (< times 1)
      new_value
      (fib_ (+ new_value old_value) new_value (- times 1))))
  (fib_ 0 1 x))

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
    (println (str (fib n) "\t" (fib (+ n 10))))))

