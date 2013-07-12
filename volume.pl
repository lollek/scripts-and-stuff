#! /usr/bin/env perl

# Update 2013-03-04 - Olle K
# - Remade script from bash into perl
# Update 2013-07-12 - Olle K
# - Script stopped working - changed $vol[32] to $vol[31]

use 5.14.2;
use strict;
use warnings;

sub vol_get {
    my @vol = split(/ /, qx(amixer sget Master | xargs));
    print("$vol[31]\n");
}
sub vol_set {
    my @vol = split(/ /, qx(amixer sset Master $ARGV[0]% | xargs));
    print("$vol[31]\n");
}
sub vol_pp {
    my @vol = split(/ /, qx(amixer sset Master 10%+ | xargs));
    print("$vol[31]\n");
}
sub vol_mm {
    my @vol = split(/ /, qx(amixer sset Master 10%- | xargs));
    print("$vol[31]\n");
}
sub vol_mute {
    my @vol = split(/ /, qx(amixer sget Master | xargs));
    if ($vol[32] eq "[on]\n") { @vol = qx(amixer sset Master mute); }
    elsif ($vol[32] eq "[off]\n") { @vol = qx(amixer sset Master unmuted); }
    else { print("Vol is " . $vol[32] . ", thus dunno what to do"); }
}
        

if (!$ARGV[0]) { &vol_get(); }
elsif ($ARGV[0] eq "++") { &vol_pp(); }
elsif ($ARGV[0] eq "--") { &vol_mm(); }
elsif ($ARGV[0] eq "mute") { &vol_mute(); }
elsif ($ARGV[0] =~ /^\d+$/) { &vol_set(); }
else {
    print("Usage: ./vol  ++ | -- | 0-100\n");
    print("\nOptions:\n");
    print("  ++    -  Set volume to +10%\n");
    print("  --    -  Set volume to -10%\n");
    print("  mute  -  Toggle mute on/off\n");
    print("  0-100 -  Set volume to 0-100%\n");
    print("  empty -  Get volume\n");
}   

=pod TAIL_INFO
Name: Volume
Language: Perl
State: Done

Change the volume (through amixer)



Example: ./volume.pl 100
=cut
