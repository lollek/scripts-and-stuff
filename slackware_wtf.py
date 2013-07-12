#! /usr/bin/env python2.7

from subprocess import check_output, PIPE
from sys import argv

if len(argv) == 3 and argv[1] == "is":

    try: print "%s" % check_output(["whatis", argv[2]], stdin=PIPE, stderr=PIPE),
    except: print "Gee...  I don't know what %s means..." % argv[2]

else: print("Usage: wtf is <something>")

""" TAIL INFO:
Name: Wtf is ..
Language: Python2.7
State: Done

I found an application called wtf on my slackware box which worked like this:
[18:10]<iix@kilo>% wtf is calendar
Gee...  I don't know what calendar means...
[18:10]<iix@kilo>% wtf is cal
cal: cal []               (1)  - displays a calendar
I found it funny, so this is a wrapper for whatis to emulate that
"""
