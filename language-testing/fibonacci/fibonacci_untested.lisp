(defun fib (n)
  "Returns the nth fibonacci"
  (defun tail (new old n)
    "Tail recursive fibonacci"
    (if (< n 1)
      new
      (tail (+ new old) new (- n 1))))
  (tail 0 1 n))

(let ((n (nth 0 *args*)))
  (if (null n)
    (loop for x from 0 to 9 do
      (format t "~D~A~D~%" (fib x) #\Tab (fib (+ x 10))))
    (format t "~D" (fib (parse-integer n)))))
