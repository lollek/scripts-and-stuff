#! /usr/bin/env perl

use strict;
use warnings;
use IO::Socket;

sub send_and_receive_data {
  my $sock = new IO::Socket::INET (
    PeerAddr => $_[0],
    PeerPort => $_[1],
    Proto => "tcp"
    );
  die "Failed to connect: $!\n" unless $sock;
  print $sock $_[2];
  while (<$sock>) {
    print $_;
  }
  close($sock);
}

my $usage = "Usage: $0 hostname port";
die "$usage\nError: No hostname given!\n" unless $ARGV[0];
die "$usage\nError: No port given!\n" unless $ARGV[1];
die "$usage\nError: Port contains non-digits!\n" unless $ARGV[1] =~ /^\d+$/;
&send_and_receive_data($ARGV[0], $ARGV[1], <STDIN>);
