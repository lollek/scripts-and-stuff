#lang racket

(printf "Guess-a-number game!~%~
         I am thinking of a number between 1 and 100~%~
         You have 5 tries to guess it correctly or I win~%~
         What's your guess?~%")

(define target (+ 1 (random 100)))

(define (guess n)
  "Guess the target number. Returns true if correct"
  (printf "Guess ~a: " n)
  (printf "Target: ~a~%" target)
  (let* ([guess-string (read-line)]
         [guess-number (string->number guess-string)])
    (cond
      ((not guess-number)      (printf "~a is not a number~%" guess-string) #f)
      ((= guess-number target) (println "Correct! You have won!") #t)
      ((= n 5)                 (printf "Haha, I won! The number was ~a~%" target) #f)
      ((< guess-number target) (println "Too low! Try again!") #f)
      ((> guess-number target) (println "Too high Try again!") #f)
      (else                    (println "Incorrect! Try again") #f))))

(void
  (for/or ([i (in-range 1 6)])
    (guess i)))

