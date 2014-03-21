(defn quicksort [x]
  (if (= (count x) 0)
    []
    (let [head (first x)
          tail (rest x)]
      (concat (quicksort (filter #(<= (compare % head) 0) tail))
              [head]
              (quicksort (filter #(> (compare % head) 0) tail))))))

(println (quicksort '(1 5 2 3 4 9 9 5)))
(println (quicksort '(\a \b \q \a \g \i)))
(println (quicksort '("a" "b" "q" "a" "g" "i")))

