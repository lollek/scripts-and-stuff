(defn paste [filename1 filename2 delim]
  (defn safe-head [x]
    (if (nil? x) "" (first x)))
  (with-open [file1 (clojure.java.io/reader filename1)
              file2 (clojure.java.io/reader filename2)]
    (loop [line1 (line-seq file1)
           line2 (line-seq file2)]
      (if (or line1 line2)
        (do
          (printf "%s%s%s\n" (safe-head line1) delim (safe-head line2))
          (recur (next line1) (next line2)))))))

(defn parse-args [a b c]
  "Finds the delimiter (e.g. -d;)"
  (cond
    (= (subs a 0 2) "-d") (paste b c (subs a 2))
    (= (subs b 0 2) "-d") (paste a c (subs b 2))
    (= (subs c 0 2) "-d") (paste a b (subs c 2))
    :else (println "Usage: ./paste FILE1 FILE2 [-d DELIM]")))

(let [argv *command-line-args*
      argc (count argv)]
  (cond
    (= argc 2) (paste (nth argv 0) (nth argv 1) " ")
    (= argc 3) (parse-args (nth argv 0) (nth argv 1) (nth argv 2))
    :else (println "Usage: ./paste FILE1 FILE2 [-d DELIM]")))
