#! /bin/sh
":"; exec emacs --no-site-file --script "$0" -- "$@" # -*-emacs-lisp-*-

(defun fib (x y z)
  (if (zerop z) 
      x
    (fib (+ x y) x (- z 1))))
  
(cond 
 ((= (length argv) 1)
  (dotimes (i 10 nil)
    (message (format "%d\t%d" (fib 0 1 i) (fib 0 1 (+ i 10))))))
 ((= (length argv) 2)
  (message (number-to-string (fib 0 1 (string-to-number (elt argv 1))))))
 (t 
  (message "Usage: ./fibonacci.el <number>")))

(setq argv nil)

;; TAIL INFO:
; Name: Fibonacci Sequence
; Language: Emacs Lisp
; State: Done
;
; Prints out numbers form the fibonacci sequence
;
;
; Example: ./fibonacci.el
; Example2: ./fibonacci.el 42
;
