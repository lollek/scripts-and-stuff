#! /usr/bin/env python3

import sys
import argparse
import socket

def send_receive_data(hostname, port, content):

    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect((hostname, port))
    s.send(content.encode())
    data = b""
    while 1:
        tmp = s.recv(1024)
        if not tmp: break
        data += tmp
    s.close()
    data = data.decode()
    print(data, end="\n" if data[-1] != "\n" else "")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Send string to server")
    parser.add_argument("hostname", help="hostname to send to")
    parser.add_argument("port", type=int, help="port to use")
    args = parser.parse_args()

    send_receive_data(args.hostname, args.port, sys.stdin.read())
