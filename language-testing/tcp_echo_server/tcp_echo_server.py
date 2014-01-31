#! /usr/bin/env python3

import socket
import argparse

def start_echo_server(port):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    s.bind(("", port))
    s.listen(1)
    while 1:
        client, _ = s.accept()
        data = client.recv(1024)
        client.sendall(data)
        client.close()
        with open("tcp_echo_server.log", "a") as f:
            f.write(data.decode())

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="TCP Echo Server")
    parser.add_argument("port", type=int, help="port to bind on")
    args = parser.parse_args()
    start_echo_server(args.port)
