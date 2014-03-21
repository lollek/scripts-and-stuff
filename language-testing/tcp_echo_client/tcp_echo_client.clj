(defn send-and-receive [hostname port contents]
  "Sends contents to remote host and listens to reply"
  (with-open [sock (new java.net.Socket hostname port)
              in (clojure.java.io/reader sock)
              out (clojure.java.io/writer sock)]
    (.write out contents)
    (.write out "\n")
    (.flush out)
    (doseq [line (line-seq in)]
      (println line))))

(if *command-line-args*
  (if (= (count *command-line-args*) 2)
    (let [[host port] *command-line-args*
          data (clojure.string/join
                 "\n" (line-seq (java.io.BufferedReader. *in*)))]
      (send-and-receive host (read-string port) data)))
  (println "Usage: echo \"DATA TO SEND\" | ./tcp_echo_client HOSTNAME PORT"))
    
