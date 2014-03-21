(defn send-and-receive [hostname port contents]
  "Sends contents to remote host and listen to reply"
  (let [host_addr (. java.net.InetAddress (getByName hostname))]
    (with-open [sock (new java.net.DatagramSocket)]
      (let [contents_bytes (. contents getBytes)]
        (. sock (send (new java.net.DatagramPacket
                      contents_bytes
                      (count contents_bytes)
                      host_addr
                      port))))
      (let [buf (byte-array 1024)
            recv_packet (new java.net.DatagramPacket buf 1024)]
        (. sock (receive recv_packet))
        (println (apply str (map char (seq (. recv_packet getData)))))))))

(if *command-line-args*
  (if (= (count *command-line-args*) 2)
    (let [[host port] *command-line-args*
          data (clojure.string/join
                 "\n" (line-seq (java.io.BufferedReader. *in*)))]
      (send-and-receive host (read-string port) data)))
  (println "Usage: echo \"DATA TO SEND\" | ./udp_echo_client HOSTNAME PORT"))

