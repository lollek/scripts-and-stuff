(import '(java.net.Socket))

(defn send-and-receive [hostname port contents]
  "Sends contents to remote host and listens to reply"
  (with-open [sock (new java.net.Socket hostname port)
              in (new java.io.BufferedReader
                      (new java.io.InputStreamReader
                           (. sock getInputStream)))
              out (clojure.java.io/output-stream (. sock getOutputStream))]
    (.write out (. contents (getBytes "UTF-8")))
    (.flush out)
    (loop [data (. in readLine)]
      (if (not (nil? data))
        (do
          (println data)
          (recur (. in readLine)))))))

(if *command-line-args*
  (if (= (count *command-line-args*) 2)
    (let [[host port] *command-line-args*
          data (clojure.string/join
                 "\n" (line-seq (java.io.BufferedReader. *in*)))]
      (send-and-receive host (read-string port) data)))
  (println "Usage: echo \"DATA TO SEND\" | ./tcp_echo_client HOSTNAME PORT"))
    
