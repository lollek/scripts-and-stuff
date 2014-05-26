(defn rot13 [x]
  "Takes a string and does rot13 'encryption' on it"
  (defn rot_char [c]
    (let [low (compare c \a)
          high (compare c \A)]
      (cond
        (<=  0 low  12) (char (+ (int c) 13))
        (<= 13 low  25) (char (- (int c) 13))
        (<=  0 high 12) (char (+ (int c) 13))
        (<= 13 high 25) (char (- (int c) 13))
        :else c)))
  (apply str (map rot_char x)))

(if *command-line-args*
  (println (clojure.string/join " " (map rot13 *command-line-args*)))
  (doseq [line (line-seq (java.io.BufferedReader. *in*))]
    (println (rot13 line))))
