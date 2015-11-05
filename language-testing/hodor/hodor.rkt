#lang racket

(define (hodor line)
  "Translate all [a-zA-Z0-9]+ into 'hodor'"
  (regexp-replace* #rx"[a-zA-Z0-9]+" line "hodor"))

(let ([args (current-command-line-arguments)])
  (if (empty? (vector->list args))
    (let loop ()
      (with-handlers ([exn:fail? (lambda (_) (void))])
        (displayln (hodor (read-line)))
        (loop)))
    (displayln (hodor (string-join (vector->list args))))))

