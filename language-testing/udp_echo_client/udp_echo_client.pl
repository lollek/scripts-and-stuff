#! /usr/bin/env perl

use strict;
use warnings;
use IO::Socket;

sub send_receive_data {
  my $sock = new IO::Socket::INET (
    PeerAddr => "$_[0]:$_[1]",
    Proto => "udp",
  ) or die "Socket error: $!\n";

  print $sock $_[2];
  my $data;
  $sock->recv($data, 1024);
  print $data;
  close $sock;
}

my $usage = "Usage: $0 hostname port";
die "$usage\nError: No hostname provided!" unless $ARGV[0];
die "$usage\nError: No port provided!" unless $ARGV[1];
die "$usage\nError: Port contains non-digits!" unless $ARGV[1] =~ /^\d+$/;
&send_receive_data($ARGV[0], $ARGV[1], <STDIN>);
