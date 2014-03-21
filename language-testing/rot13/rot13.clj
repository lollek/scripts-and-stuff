(defn rot13 [x]
  "Takes a string and does rot13 'encryption' on it"
  (defn rot_char [c]
    (let [low (compare c \a)
          high (compare c \A)]
      (cond
        (and (<= 0 low) (< low 13)) (char (+ (int c) 13))
        (and (<= 13 low) (< low 26)) (char (- (int c) 13))
        (and (<= 0 high) (< high 13)) (char (+ (int c) 13))
        (and (<= 13 high) (< high 26)) (char (- (int c) 13))
        :else c)))
  (apply str (map rot_char x)))

(if *command-line-args*
  (println (clojure.string/join " " (map rot13 *command-line-args*)))
  (doseq [line (line-seq (java.io.BufferedReader. *in*))]
    (println (rot13 line))))
