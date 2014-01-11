#! /usr/bin/env python3
# This application replaces all words with hodor.
# Usage: ./hodor.py 'hello world!'
# Usage: echo "hello world!" | ./hodor.py

from re import sub
from sys import argv, stdin

def hodor(line):
    print(sub("\w+", "hodor", line), end="\n" if line[-1] != '\n' else "")

def main():
    if len(argv) > 1:
        hodor(" ".join(argv[1:]))
    else:
        for line in stdin:
            hodor(line)

if __name__ == "__main__":
    main()
