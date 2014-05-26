(defn quicksort [x]
  (if (< 1 (count x))
    (concat (quicksort (filter #(<= (compare % (first x)) 0) (rest x)))
            [(first x)]
            (quicksort (filter #(> (compare % (first x)) 0) (rest x))))
    x))

(println (quicksort '(1 5 2 3 4 9 9 5)))
(println (quicksort '(\a \b \q \a \g \i)))
(println (quicksort '("a" "b" "q" "a" "g" "i")))

