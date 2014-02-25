#! /usr/bin/env perl

use strict;
use warnings;

use IO::Handle;
use IO::Socket;
$| = 1;

sub start_echo_server {
  my $sock = new IO::Socket::INET (
    LocalPort => $_[0],
    Proto => "udp",
    Reuse => 1
  ) or die "Failed to start socket: $!\n";

  for (;;) {
    my $buf;
    $sock->recv($buf, 1024);
    $sock->send($buf);

    # Log the connection
    my $hostname = $sock->peerhost();
    open my $log, ">>", "udp_echo_server.log" or die "open: $!\n";
    print $log $buf;
    close $log;
  }
}

if ($#ARGV == 0) {
  die "Error: Port contains non-digits!" unless $ARGV[0] =~ /^\d+$/;
  &start_echo_server($ARGV[0]);
} else {
  print "Usage: $0 port\n";
  exit 1;
}
