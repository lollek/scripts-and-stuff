#! /usr/bin/env perl

use 5.14.2;
use strict;
use warnings;

sub fib {

    my $oldval = 1;
    my $newval = 0;
    my $tmp = 0;

    while (--$_[0] >= 0) {
        $tmp = $oldval;
        $oldval = $newval;
        $newval = $tmp + $oldval;
    }
    return $newval;
}

# Fib the arugment, if possible:
if ($#ARGV == 0) {
    printf "%d\n", &fib($ARGV[0]);
    exit 0;
}
# Otherwise, run 20 fibs
for (0..9) {
    printf "%d\t%d\n", &fib($_+0), &fib($_+10);
}

=pod TAIL INFO
Name: Fibonacci Sequence
Language: Perl
State: Done
Created: 2013-08-08

Prints out numbers from the fibonacci sequence

Example1: ./fibonacci.pl
Example2: ./fibonacci.pl 42
=cut
