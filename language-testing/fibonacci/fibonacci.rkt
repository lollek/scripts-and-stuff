#lang racket

(define (fib n)
  "Returns the nth fibonacci"
  (define (fib-tail new old n)
    "Tail recursive fibonacci"
    (if (< n 1)
      new
      (fib-tail (+ new old) new (- n 1))))
  (fib-tail 0 1 n))

(let ([args (current-command-line-arguments)])
  (if (= (vector-length args) 1)
    (let* ([arg1-string (vector-ref args 0)]
           [arg1-number (string->number arg1-string)])
      (if arg1-number
        (printf "~a~%" (fib arg1-number))
        (printf "~a: Not a number~%" arg1-string)))
    (for ([i 10])
      (printf "~a\t~a~%" (fib i) (fib (+ i 10))))))
