#! /usr/bin/env perl

# v0.1 - Created 2013-02-18 - Olle K
# My first attempt at Perl, and is to substitute my python script for auto-wlan-connection

# v0.2 - Edit 2013-03-04 - Olle K
# - Now scans a dir for .psk files instead of using a .known-file

# v0.2.1 - Edit 2013-03-12 - Olle K
# - Added kill argument which kills all wpa_supplicant and dhclient in ps-tree

# v0.3 - Edit 2013-03-19 - Olle K
# - Rewrote whole script, looks much nicer now with the same functions. Plus:
# - Removed all globals except for one
# - Made it super easy to add a 'select which AP to connect to' for the future

# v0.3.1 - Edit 2013-03-21 - Olle K
# - Now you can select AP to connect to after scanning. Only works for non-encrypted AP's \
# or when the passphrase is known
# - Added a nice tail /var/log/syslog while connecting
#   ^ Removed tail later and used dhclient -v instead
# - Made script strict and warnings compliant
# - Wlan Kill now sets wlan0 to 'up' again

# v0.3.2 - Edit 2013-04-13 - Olle K
# - Can now connect to unknown "red" (protected) Access Points if you provide password
#   Passwords are automatically saved, even if it does not work

# v0.3.3 - Edit 2013-05-17 - Olle K
# - Changed kill-process to use killall instead

# Check if it's run as root, otherwise exit
if ($> != 0) { print "You need to run this script as root\n"; exit() }

use 5.14.2;
use strict;
use warnings;
use Env;

sub wlan_scan {

# Make a nice list of all known APs:
    opendir(my $DIR, "$HOME/wlan") or die "$HOME/wlan/ not found!\
create an empty directory or modify the script";
    my @KNOWN = readdir($DIR);
    closedir($DIR);

    print("Searching for APs ... ");
# Scan and Sort: 
    # Get result from 'iwlist wlan0 scan' and sort by 'Cell'
    # Remove the first result since it's not an AP
    # Keep track of length of largest ESSID (for formating)
    my @wlan0scan = split('Cell', qx(iwlist wlan0 scan));
    shift @wlan0scan;
    our $maxessidlen = 0;                                     

    # Sort every 'Cell' (= AP) found:
    # Add all to the nice CONNLIST, also sort connection by:
    # 2 = known, 1 = no encryption, 0 = encrypted and unknown
    my @CONNLIST;
    my $i = 0;
    foreach (@wlan0scan) {
        $i += 1;
        my @splice = split("\n", $_);
        
        my $mac = substr($splice[0], 15);
        my $q = substr($splice[3], 28, 2);
        my $enc = substr($splice[4], 35);
        my $essid = substr($splice[5], 26);
        my $essidlen = length $essid;
        $essid = substr($essid, 1, $essidlen-2);

        if ($essid ~~ @KNOWN) { $enc = 2; }
        elsif ($enc eq "off") { $enc = 1; }
        else {$enc = 0;}
        
        push(@CONNLIST, [$i, $essid, $mac, $q, $enc]);

        if (length $essid > $maxessidlen) { $maxessidlen = length $essid; }
    }
    return @CONNLIST;
}

sub wlan_echo {
    
# Print out all APs from &wlan_scan. Also return them if we want to do some more
    our $maxessidlen;
    my $conn_no = scalar @_;
    if ($conn_no == 0 ) { print("0 found.\n"); return; }
    else {
        printf("%d found:\n", $conn_no);
        for my $conn (@_) {
            if (@$conn[4] == 0) { print("\e[1;31m"); }
            elsif (@$conn[4] == 1) { print("\e[1;34m"); }
            elsif (@$conn[4] == 2) { print("\e[1;32m"); }
            printf("\t%02d: %${maxessidlen}s | Q: %s/70\n\e[0m", @$conn[0], @$conn[1], @$conn[3]);
        }
        print("\n");
    }
    return @_;
}

sub wlan_find_best {
    
# Find best network from CONNLIST and return it
    my @best = ();
    for my $i (@_) {
        if (scalar @best == 0) { @best = @$i; }
        elsif (@$i[4] > $best[4]) { @best = @$i; }
        elsif ((@$i[4] == $best[4]) && (@$i[3] > $best[3])) { @best = @$i; }
    }
    if ($best[4] == 0) { @best = (); }
    return @best;
}

sub wlan_select {
    my $conn_no = scalar @_;
    if ($conn_no == 0) { return; }
    while (1) {
        printf("Which do you want to connect to? (1-%d, ^C to exit)? ", $conn_no);
        my $best = <>;
        if ((1 <= $best) && ($best <= $conn_no)) {
            my $connection = &wlan_connect(@ {$_[$best-1]});
            if ($connection == 0) { return; }
        } else { print("Unknown choice: " . $best); }
    }
}


sub wlan_connect {
    
# Connect to wanted AP
    # If given list is empty:
    if (scalar @_ == 0) {
        print "No good AP found..\n";
        return 1;
        
    # If the access point is known:
    } elsif ($_[4] == 2) {
        print "Connecting to \e[1;32m" . $_[1] . "\e[0m..\n\n";

        my $ret = system("wpa_supplicant -B -D nl80211 -i wlan0 -c /root/wlan/$_[1]");
        if ($ret != 0) {
            print("wpa_supplicant failed with error code: " . $ret);
            return $ret;
        }
        #if (($ret = &wlan_connect_dh()) == 0) {
        #    print("Connected to \e[1;32m" . $_[1] . "\e[0m\n");
        #}
        return &wlan_connect_dh();
        
    # If there's no encryption:
    } elsif ($_[4] == 1) {
        print "Connecting to \e[1;34m" . $_[1] . "\e[0m..\n\n";

        my $ret = system("iwconfig wlan0 essid $_[1]");
        if ($ret != 0) {
            print("Failed to set essid with code: " . $ret);
            return $ret;
        }
        #if (($ret = &wlan_connect_dh()) == 0) {
        #    print("Connected to \e[1;34m" . $_[1] . "\e[0m\n");
        #}
        return &wlan_connect_dh();
        
    # Encrypted and unknown:
    } else {
        print "\e[1;31m" . $_[1] . "\e[0m is password protected. Enter password: ";
        my $passwd = <>;
        my @psk_file = qx(wpa_passphrase $_[1] $passwd);
        open(my $psk_file_f, ">", "/root/wlan/$_[1]") or die "Unable to open file /root/wlan/$_[1]: $!";
        print $psk_file_f @psk_file;
        close $psk_file_f or die "$_[1]: $!";
        system("wpa_supplicant -B -D nl80211 -i wlan0 -c /root/wlan/$_[1]; dhclient wlan0 -v");
        return 0;
    }
}
sub wlan_connect_dh {
    my $ret = system("dhclient wlan0 -v");
    if ($ret != 0) {
        print("Failed to associate with access point\n");
        &wlan_killall();
    }
    return $ret;
}

sub wlan_killall {
# Kill wpa_supplicant and dhclient

    print("Killing dhclient and wpa_supplicant .. ");
    system("killall dhclient wpa_supplicant &> /dev/null");
    print("done\nSetting wlan0 to up .. ");
    sleep 1;
    system("ifconfig wlan0 up");
    print("done\n");
}




# Print header:
print("\e[4mLollian WLAN Scan v0.3.3\e[0m\n");
print("Created: 2013-02-18 / Updated: 2013-05-20\n\n");

# Execute subs depending on argument:
if    (!$ARGV[0])          { &wlan_select(&wlan_echo(&wlan_scan)); }
elsif ($ARGV[0] eq 'list') { &wlan_echo(&wlan_scan); }
elsif ($ARGV[0] eq 'auto') { &wlan_connect(&wlan_find_best(&wlan_echo(&wlan_scan))); }
elsif ($ARGV[0] eq 'kill') { &wlan_killall; }
else {
    print("Usage: wlan auto|kill|list\n");
    print("\nOptions:\n");
    print("  auto  -  autoconnect to the most suitable AP\n");
    print("  kill  -  kill wpa_supplicant and dhclient\n");
    print("  list  -  list wireless lans in the vicinity\n");
}                   
=pod TAIL_INFO
Name: Wlan
Language: Perl
State: Done w/bugs - unpolished (Should port to python/ruby and fix bugs..)

Connect to access points easier



Example: ./wlan.pl auto
=cut
