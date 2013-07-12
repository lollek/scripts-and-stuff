#! /usr/bin/env python3.2
""" Olle K - 2013-07-09 """

import sys
import socket
import json

class Searcher:
    
    def __init__(self, subject):
        self.subject = subject
        self.do_search()

    def do_search(self):
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.connect(("duckduckgo.com", 80))
        sock.send(("GET /?q=%s&format=json\r\n\r\n" % self.subject).encode())
        self.json = b""
        while 1:
            tmp = sock.recv(1024)
            if not tmp: break
            self.json += tmp
        sock.close()
        self.sort_and_print()

    def sort_and_print(self):
        data = json.loads(self.json.decode())
        if data["AbstractText"]:
            print("\033[1;20m%s\033[0m" % data["AbstractText"], end="\n\n")
            
        for instance in data["RelatedTopics"]:
            for i in instance:
                if i == "Text":
                    desc = instance["Text"]
                    print(desc)

        if not data["AbstractText"] and not data["RelatedTopics"]:
            print("Gee.. I don't know what that is")
        

if __name__ == "__main__":

    if len(sys.argv) >= 3 and sys.argv[1] == "is":
        subject = " ".join(sys.argv[2:])
    elif len(sys.argv) == 2:
        subject = sys.argv[1]
    else: sys.exit()

    Searcher(subject)

""" TAIL INFO:
Name: WTF is ..
Language: Python3.2
State: Done - but could be improved

Uses the duckduckgo.com API for finding out what stuff means



Example: ./wtf.py is fibonacci 
"""
