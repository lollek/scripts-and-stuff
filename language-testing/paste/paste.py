#! /usr/bin/env python3

import argparse

def paste(filename1, filename2, delim):
    with open(filename1) as file1, open(filename2) as file2:
        while 1:
            line1 = file1.readline()
            line2 = file2.readline()
            if line1 or line2:
                print(line1.rstrip(), line2.rstrip(), sep=delim) 
            else: break

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Combine two files")
    parser.add_argument("file1", help="file to the left")
    parser.add_argument("file2", help="file to the right")
    parser.add_argument("-d", default=' ', dest="delim", help="delimiter")
    args = parser.parse_args()

    paste(args.file1, args.file2, args.delim)

""" TAIL INFO:
Name: Paste
Language: Python3
State: Done

This is an answer to a python-test question at my school, basically:
It should take two files as argument and pastes them with a delimiter in between.
It should be possible to change the delimiter with -d
The files must be read line-by-line
Example: ./paste.py A.TXT B.TXT -d1
"""
