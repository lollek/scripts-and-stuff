#! /usr/bin/env perl

use 5.14.2;
use strict;
use warnings;

sub fib {
  my $oldval = 1;
  my $newval = 0;
  my $tmp = 0;
  for (1..$_[0]) {
    $tmp = $oldval;
    $oldval = $newval;
    $newval = $tmp + $oldval;
  }
  return $newval;
}

sub test {
  print "Running test-cases\n";
  &fib(0) == 0 or die "Assert failed";
  &fib(1) == 1 or die "Assert failed";
  &fib(2) == 1 or die "Assert failed";
  &fib(42) == 267914296 or die "Assert failed";
  print "Success!\n";
  exit 0
}

# Fib the arugment, if possible:
if ($#ARGV == 0) {
  &test() if $ARGV[0] eq "test";
  printf "%d\n", &fib($ARGV[0]);
# Otherwise, run 20 fibs
} else {
  for (0..9) {
    printf "%d\t%d\n", &fib($_), &fib($_+10);
  }
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
