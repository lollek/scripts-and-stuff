#! /usr/bin/env guile
!#

(use-modules
  (ice-9 format)
  (ice-9 rdelim))

(display "Guess-a-number game!\n")
(display "I am thinking of a number between 1 and 100\n")
(display "You have 5 tries to guess it correctly or I win\n")
(display "What's your guess?\n")

(set! *random-state* (random-state-from-platform))
(define target (+ (random 100) 1))
(display target)

(define (guess i)
  (define (correct) (display "Correct! You won!\n") #t)
  (define (too what) (format #t "Too ~a Try again!\n" what) #f)
  (define (lose) (format #t "Haha, I won! The number was ~d" target) #t)

  (format #t "Guess ~d: " i)
  (let ((guess (string->number (read-line))))
    (format #t "Guess: ~d~%" guess)
    (cond
      (= guess target) (correct)
      (< guess target) (too "low")
      (> guess target) (too "high")
      (= i 5)          (lose))))

(do ((i 1 (+ i 1)))
; ((guess i))
  (#f)
  (format #t "Return value: ~a~%" (guess i)))
