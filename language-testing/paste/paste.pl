#! /usr/bin/env perl

use strict;
use warnings;

use Getopt::Long;
our $delim = " ";

sub parse {
  open my $file1, $ARGV[0] or die "Unable to open $ARGV[0]: $!";
  open my $file2, $ARGV[1] or die "Unable to open $ARGV[1]: $!";

  while (1) {
    my $line1 = <$file1>;
    my $line2 = <$file2>;
    
    if (not defined $line1 and not defined $line2) {
      close $file1;
      close $file2;
      return;
    }

    if (defined $line1) {
      chomp($line1);
      print $line1;
    }

    print $delim;

    if (defined $line2) {
      chomp($line2);
      print $line2;
    }
    print "\n";
  }
}

GetOptions("d=s"=>\$delim);
if ($#ARGV == 1) {
  &parse;
}

=pod TAIL INFO
Name: Paste
Language: Perl
State: Done

This is an answer to a python-test question at my school, basically:
It should take two files as argument and paste them with a delim in between.
It should be possible to change delim with -d
The files must be read line-by-line
Example: ./paste.pl A.TXT B.TXT -d 1
=cut
