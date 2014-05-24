;; Helper function
(defn calculate-cubic [p, q, r, s, x]
  "Returns px^3+qx^2+rx+s"
  (+ (* p (reduce * (repeat 3 x)))
     (* q (reduce * (repeat 2 x)))
     (* r x)
     s))

(defn calculate-derivata [p, q, r, x]
  "Returns 3px^2+2qx+r"
  (+ (* p (reduce * (repeat 2 x)) 3)
     (* q x 2)
     r))

(defn abs [num]
  "Returns num as a positive value"
  (if (<= 0 num)
    num
    (- 0 num)))

(defn fetch-num [query]
  "Returns a num from user"
  (print query)
  (flush)
  (read-string (read-line)))


;; Main functions
(defn interval-halving [p, q, r, s, a, b, max-permitted-diff]
  "Solves px^3+qx^2+rx+s = 0"

  ; Loop until the diff between a and b is less than the max permitted one
  ; or f(x) = 0 is found
  (defn inner-fun [p q r s a b max-permitted-diff]
    (loop [i 1 a a b b]
      (let [c (/ (+ b a) 2)
            calculated-c (calculate-cubic p q r s c)
            current-diff (/ (- b a) 2)]
        (cond
          (or (= calculated-c 0)
              (< current-diff max-permitted-diff))
            (printf "Interval halving: x is %f +/- %f after %d iterations\n"
                    (double c) (double current-diff) i)
          (> calculated-c 0) (recur (inc i) a c)
          (< calculated-c 0) (recur (inc i) c b)))))

  ; Make sure x really is between a and b, we want a <= x <= b
  (let [test-a (calculate-cubic p q r s a)
        test-b (calculate-cubic p q r s b)]
    (cond
      (and (<= test-a 0) (>= test-b 0))
        (inner-fun p q r s a b max-permitted-diff)
      (and (>= test-a 0) (<= test-b 0))
        (inner-fun p q r s b a max-permitted-diff)
      :else
        (printf "There is no f(x)=0 between a(%f) and b(%f)!\n"
                (double test-a) (double test-b)))))


(defn newton-raphson [p, q, r, s x0, max-permitted-diff]
  "Solves px^3+qx^2+rx+s = 0"
  (loop [i 1 c x0]
    (let [dc (- c
                (/ (calculate-cubic p q r s c)
                   (calculate-derivata p q r c)))
          current-diff (/ (abs (- dc c)) (abs dc))]
      (if (< current-diff max-permitted-diff)
        (printf "Newton-Raphson: x is %f +/- %f after %d iterations\n"
                (double dc) (float current-diff) i)
        (recur (inc i) dc)))))

;; Main
(println "Solve px³+qx²+rx+s = 0")
(let [p (fetch-num "Provide p: ")
      q (fetch-num "Provide q: ")
      r (fetch-num "Provide r: ")
      s (fetch-num "Provide s: ")
      a (fetch-num "Provide a for interval halving: ")
      b (fetch-num "Provide b for interval halving: ")
      x (fetch-num "Provide x0 for Newton-Raphson: ")
      n (fetch-num "Provide decimal precision: ")
      max-permitted-diff (/ 1 (reduce * (repeat n 10)))]
  (interval-halving p q r s a b max-permitted-diff)
  (newton-raphson p q r s x max-permitted-diff))
