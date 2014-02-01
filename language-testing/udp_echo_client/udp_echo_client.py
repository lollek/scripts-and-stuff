#! /usr/bin/env python3

import sys
import socket
import argparse

def send_receive_data(hostname, port, content):
    with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as sock:
        sock.sendto(content.encode(), (hostname, port))
        data, client = sock.recvfrom(1024)
        data = data.decode()
        print(data, end='' if data[-1] == '\n' else '\n')

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="UDP Echo Client")
    parser.add_argument("hostname", help="target hostname")
    parser.add_argument("port", type=int, help="target port")
    args = parser.parse_args()
    send_receive_data(args.hostname, args.port, sys.stdin.read())
