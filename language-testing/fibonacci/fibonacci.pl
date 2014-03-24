#! /usr/bin/env perl
# Prints out numbers from the fibonacci sequence
# Example usage: ./fibonacci.pl
# Example usage: ./fibonacci.pl 42

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

# Fib the arugment, if possible:
# Otherwise, run 20 fibs
unless (caller) {
  if ($#ARGV == 0) {
    printf "%d\n", &fib($ARGV[0]);
  } else {
    for (0..9) {
      printf "%d\t%d\n", &fib($_), &fib($_+10);
    }
  }
}

