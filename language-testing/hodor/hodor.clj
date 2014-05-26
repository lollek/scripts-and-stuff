
(defn hodor [line]
  (clojure.string/replace line #"[a-zA-Z0-9]+" "hodor"))

(if *command-line-args*
  (println (clojure.string/join " " (map hodor *command-line-args*)))
  (doseq [line (line-seq (java.io.BufferedReader. *in*))]
    (println (hodor line))))

