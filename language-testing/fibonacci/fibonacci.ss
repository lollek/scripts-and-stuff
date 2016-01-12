#! /usr/bin/env guile
!#

(define (fib n)
  "Returns the nth fibonacci"
  (define (fib-tail new old n)
    "Tail recursive fibonacci"
    (if (< n 1)
      new
      (fib-tail (+ new old) new (- n 1))))
  (fib-tail 0 1 n))

(define (fib-arg1)
  (let*
    ((arg      (list-ref (program-arguments) 1))
     (arg-num  (string->number arg)))
    (if arg-num
      (format #t "~a~%" (fib arg-num))
      (format #t "~a: Not a number~%" arg))))

(define (fibs n)
  (do ((i 0 (+ i 1)))
    ((= n i))
    (format #t "~a\t~a~%" (fib i) (fib (+ i 10)))))

(if (= 2 (length (program-arguments)))
  (fib-arg1)
  (fibs 10))

