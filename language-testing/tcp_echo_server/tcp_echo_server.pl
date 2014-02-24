#! /usr/bin/env perl

use strict;
use warnings;

use IO::Socket;
$| = 1;

sub start_echo_server {
  my $sock = new IO::Socket::INET (
    LocalPort => $_[0],
    Proto => "tcp",
    Listen => 5,
    Reuse => 1
  ) or die "Failed to start socket: $!\n";

  for (;;) {
    my $client = $sock->accept() or die "Accept: $!\n";
    my $data = <$client>;
    print $client $data;
    close $client;
    open my $f, ">>", "tcp_echo_server.log" or die "open: $!\n";
    print $f $data;
    close $f;
  }
}

if ($#ARGV == 0) {
  &start_echo_server($ARGV[0]);
} else {
  print "Usage: $0 port\n";
  exit 1;
}
