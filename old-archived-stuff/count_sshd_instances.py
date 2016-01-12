#! /usr/bin/env python2.7

from subprocess import check_output

""" Find all IP-addresses in /var/log/sshd: """
raw_ips = check_output("grep -o \([0-9]\{1,3\}\.\)\{3\}[0-9]\{1,3\} /var/log/sshd".split()
                       ).split("\n")

""" Remove all copies: """
ips = list(set(raw_ips))

""" Print them out: """
print "IPs in /var/log/sshd error log:"
for ip in ips:
    print "%-15s: %d" % (ip, raw_ips.count(ip))
print "\n%-15s: %d" % ("Total", len(raw_ips))
