#! /usr/bin/env perl

package D4Tests;
use strict;
use warnings;

use Switch;
use Sys::Hostname;
use Exporter qw{import};
our @EXPORT = qw{title good bad
                 assert_ping
                 dns_lookup_SOA dns_lookup_A
                 dns_lookup_MX dns_lookup_reverse
                 test_DNS_LAN test_EMB_WAN
                 $domain $reverse_domain $ip_prefix $running_test
                 %server @clients};

our $network_id = "130.236.179.88/29";
our $domain = "d4.sysinst.ida.liu.se";
our $reverse_domain = "88-29.179.236.130.in-addr.arpa.";
our $ip_prefix = "130.236.179";

our $ldap_password="";
our $ldap_dc="dc=d4,dc=sysinst,dc=ida,dc=liu,dc=se";
our $ldap_admin="cn=admin,$ldap_dc";

our $running_test = "???";

our %server = (
    shortname => "server",
    longname  => "server.$domain",
    ip        => "$ip_prefix.90",
    rev_ip    => "90.$reverse_domain"
);

our @clients = (
  {
    shortname => "gw",
    longname  => "gw.$domain",
    ip        => "$ip_prefix.89",
    rev_ip    => "89.$reverse_domain"
  },
  {
    shortname => "server",
    longname  => "server.$domain",
    ip        => "$ip_prefix.90",
    rev_ip    => "90.$reverse_domain"
  },
  {
    shortname => "client-1",
    longname  => "client-1.$domain",
    ip        => "$ip_prefix.91",
    rev_ip    => "91.$reverse_domain"
  },
  {
    shortname => "client-2",
    longname  => "client-2.$domain",
    ip        => "$ip_prefix.92",
    rev_ip    => "92.$reverse_domain"
  },
);

my $hostname = hostname;

sub title { print "\n\033[0;33m$running_test: $_[0]\033[0m\n" }
sub good { print "$hostname: \033[0;32m$running_test: Success: $_[0]\033[0m\n" }
sub bad { print STDERR "$hostname: \033[0;31m$running_test: Error: $_[0]\033[0m\n" }

sub assert {
  my $description = $_[0];
  chomp(my $expected = $_[1]);
  chomp(my $real = $_[2]);

  $expected eq $real
    and good "$description: $real"
    or bad "$description returned '$real', expected '$expected'";
}

sub assert_ping {
  my $host = $_[0];
  qx{ping -c1 $host};

  if ($? == 0) {
    good "$host is online";
  } else {
    bad "$host is offline";
  }
}

sub assert_if_config {
  my $interface = $_[0];
  my $expected = $_[1];

  my $ip_addr = "[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+";
  my $ip_range = "/[0-9]+";

  my $real = qx{ip a show $interface | grep -Eo "inet $ip_addr$ip_range( brd $ip_addr)?"};
  chomp $real;
  $real eq $expected
    and good "$interface: $real"
    or bad "$interface returned '$real', expected '$expected'";
}

sub assert_time {
  my $description = "Time ";

  my $googledatetime = qx{curl -I google.com 2>/dev/null | \\grep -Po '(?<=Date:).*'};
  chomp $googledatetime;
  if ($googledatetime eq "") {
    bad "$description: Failed to get data from google.com";
    return;
  }

  chomp(my $googletime = qx{date --date="$googledatetime" "+%s"});
  chomp(my $localtime = qx{date "+%s"});
  my $timediff = $googletime - $localtime;
  $timediff = -$timediff if $timediff < 0;

  if ($timediff == 0 || $timediff == 1) {
    good "$description: Differs $timediff seconds from google";
  } else {
    bad "$description: Differs $timediff seconds from google";
  }
}

sub get_ntp_data {
  my $data;
  do {
    chomp($data = qx{ntpq -pn});
  } while ($data eq "No association ID's returned");
  return $data;
}

sub assert_num_timeservers {
  my $data = get_ntp_data;
  my $description = "Number of timeservers";
  my $real = qx[echo "$data" | awk 'NR>2 {i++} END {print i}'];
  assert $description, 1, $real;
}

sub assert_timeserver {
  my $data = get_ntp_data;
  my $description = "Correct timeserver";
  my $real = qx[echo "$data" | awk 'NR>2 {print \$1}'];
  my $expected;

  switch (substr $real, 0, 1) {
    case /^[\*#o\+x\.-]$/ { $real = substr $real, 1; }
  }

  if ($hostname eq "gw") {
    $expected = "130.236.178.1";
  } else {
    $expected = "130.236.179.89";
  }

  assert $description, $real, $expected;
}

sub dns_lookup_A {
  my $host = $_[0];
  my $expected = $_[1];
  chomp(my $real = qx{dig +short "$host" A});
  $real eq $expected
    and good "$host (A) resolves to $real"
    or bad "$host (A) resolves to $real, expected $expected";
}

sub dns_lookup_SOA {
  my $host = $_[0];
  my $expected = $_[1];
  chomp(my $real = qx(dig +short "$host" SOA | awk '{print \$1}'));
  $real eq $expected
    and good "$host (SOA) resolves to $real"
    or bad "$host (SOA) resolves to $real, expected $expected";
}

sub dns_lookup_MX {
  my $host = $_[0];
  my $expected = $_[1];
  chomp(my $real = qx{dig +short "$host" MX});
  $real eq $expected
    and good "$host (MX) resolves to $real"
    or bad "$host (MX) resolves to $real, expected $expected";
}

sub dns_lookup_TXT {
  my $host = $_[0];
  my $expected = $_[1];
  chomp(my $real = qx{dig +short "$host" TXT});
  $real eq $expected
    and good "$host (TXT) resolves to $real"
    or bad "$host (TXT) resolves to $real, expected $expected";

}

sub dns_lookup_reverse {
  my $ip = $_[0];
  my $expected = "$_[1].";
  chomp(my $real = qx{dig +short -x "$ip"});
  $real eq $expected
    and good "$ip resolves to $real"
    or bad "$ip resolves to $real, expected $expected";
}

sub ldap_assert_exists {
  my $user = $_[0];
  my $expected = $_[1];
  qx{ldapsearch -x uid=$user | grep "^uid:"};
  my $real = $? == 0;
  my $realbool = $real ? "Yes" : "No";

  if ($real == $expected) {
    good "$user exists? $realbool";
  } else {
    bad "$user exists? $realbool";
  }
}

sub ldap_add {
  my $user = $_[0];
  my $passsha = $_[1];

  my $uid = int(rand(100000));
  chomp(my $tmp_name = qx{mktemp});
  open my $tmp_file, '>', $tmp_name or die $!;
  print $tmp_file qq{
dn: uid=$user,ou=People,$ldap_dc
objectClass: top
objectClass: account
objectClass: posixAccount
objectClass: shadowAccount
cn: $user
uid: $user
uidNumber: $uid
gidNumber: 100
homeDirectory: /home/$user
loginShell: /bin/bash
gecos: Test user
userPassword: $passsha
shadowLastChange: 0
shadowMax: 99999
shadowWarning: 7
};
  close $tmp_file;

  qx{ldapadd -w "$ldap_password" -D $ldap_admin -f $tmp_name};
  if ($? == 0) {
    good "Created user $user";
  } else {
    bad "Failed to create user $user";
  }

  unlink $tmp_name;
}

sub ldap_delete {
  my $user = $_[0];
  qx{ldapdelete -w "$ldap_password" -D $ldap_admin uid=$user,ou=People,$ldap_dc};

  if ($? == 0) {
    good "Deleted user $user";
  } else {
    bad "Failed to delete user $user";
  }
}

sub expect_imap {
  my $longname = $_[0];
  my $username = $_[1];
  my $password = $_[2];

  my $command = "openssl s_client -starttls imap -crlf -connect $longname:imap";
  #my $command = "nc $longname imap";

  chomp(my $tmp_name = qx{mktemp});
  open my $tmp_file, '>', $tmp_name or die $!;
  print $tmp_file qq|
set timeout 7
spawn $command
expect {
  "OK *CAPABILITY" {
    send "a1 LOGIN $username $password\r"
    exp_continue
  }

  "OK LOGIN" {
    send "a2 LOGOUT\r"
    exp_continue
  }

  "OK LOGOUT" {
    exit 0
  }

  timeout {
    exit 2
  }
}

exit 1
|;
  close $tmp_file;
  qx{expect <$tmp_name};
  #system "expect <$tmp_name";
  my $retval = $?;
  unlink $tmp_name;
  return $retval;
}

sub assert_imap_login {
  my $longname = $_[0];
  my $username = $_[1];
  my $password = $_[2];

  my $status = expect_imap $longname, $username, $password;
  if ($status == 0) {
    good "Can login with imap on $username\@$longname";
  } else {
    my $description;
    switch ($status >> 8) {
      case 1 { $description = "Unexpected reply from server"; }
      case 2 { $description = "Connection timed out"; }
      else   { $description = "Unknown error ($status >> 8)"; }
    }
    bad "Failed to do imap login for $username\@$longname";
  }
}

sub expect_smtp {
  my $longname = $_[0];
  my $fromaddr = $_[1];
  my $toaddr = $_[2];
  my $body = $_[3];

  $body = "test" unless $body;

  my $command = "openssl s_client -starttls smtp -crlf -connect $longname:smtp";
  #my $command = "nc $longname 25";

  chomp(my $tmp_name = qx{mktemp});
  open my $tmp_file, '>', $tmp_name or die $!;
  print $tmp_file qq|
set timeout 7
spawn $command
send "ehlo test.test\r"
expect {
  "250-$longname" {
    send "mail from:<$fromaddr>\r"
    exp_continue
  }

  "250 2.1.0" {
    send "rcpt to:<$toaddr>\r"
    exp_continue
  }

  "250 2.1.5" {
    send "data\r"
    exp_continue
  }

  "354 " {
    send "From: D4Test <$fromaddr>\r"
    send "To: <$toaddr>\r"
    send "\r"
    send "$body\r.\r"
    exp_continue
  }

  "250 2.0.0" {
    send "quit\r"
    exp_continue
  }

  "221 2.0.0" {
    exit 0
  }

  "451 4.3.0" {
    exit 3
  }

  "450 4.2.0" {
    exit 4
  }

  "554 5.7.1" {
    exit 5
  }

  "550 5.1.1" {
    exit 6
  }

  "221 2.7.0" {
    exit 7
  }

  timeout {
    exit 2
  }
}

exit 1
|;
  close $tmp_file;
  my $data = qx{expect <$tmp_name};
  my $status = $? >> 8;
  unlink $tmp_name;

  my $description;
  switch ($status) {
    case 0 { $description = "Successfully sent email"; }
    case 1 { $description = "Unexpected reply from server"; }
    case 2 { $description = "Connection timed out"; }
    case 3 { $description = "Greylisted"; }
    case 4 { $description = "Mailbox unavailable (maybe greylisted?)"; }
    case 5 { $description = "Relay access denied"; }
    case 6 { $description = "Recipient address rejected"; }
    case 7 { $description = "Server thinks we did not follow protocol"; }
    else   { $description = "Unknown error ($status)"; }
  }

  return ($status, $description, $data);
}

sub assert_no_relay {
  my $longname = $_[0];

  my ($status, $description, $data) = expect_smtp $longname, "test\@testsrv.test", "me\@student.liu.se";

  if ($status == 5) {
    good "Relay: $description";
  } else {
    bad "Relay: $description\n'$data'";
  }
}

sub assert_smtp_send {
  my $longname = $_[0];
  my $fromaddr = $_[1];
  my $toaddr = $_[2];

  my ($status, $description, $data) = expect_smtp $longname, $fromaddr, $toaddr;
  if ($status == 0) {
    good "Can send email $fromaddr -> $toaddr on $longname";
  } else {
    bad "Failed to send email $fromaddr -> $toaddr on $longname ($description)\n'$data'";
  }
}

sub assert_smtp_spamfilter {
  my $longname = $_[0];
  my $fromaddr = $_[1];
  my $toaddr = $_[2];
  my $body = "XJS*C4JDBQADN1.NSBN3*2IDNEN*GTUBE-STANDARD-ANTI-UBE-TEST-EMAIL*C.34X";

  my ($status, $description, $data) = expect_smtp $longname, $fromaddr, $toaddr, $body;
  if ($status == 0) {
    good "Can send spam email $fromaddr -> $toaddr on $longname";
  } else {
    bad "Failed to send spam email $fromaddr -> $toaddr on $longname ($description)\n'$data'";
    return;
  }

  sleep 1;
  chomp(my $mailboxdata = qx{</dev/null ssh -i id_rsa_d4 root\@server.d4.sysinst.ida.liu.se mail -u olle -f ~olle/Maildir | grep -o '\\*\\*\\*\\*\\*SPAM\\*\\*\\*\\*\\*'});

  $mailboxdata eq "*****SPAM*****"
    and good "Spamfilter: Found email from spamfilter"
    or bad "Spamfilter: Found no email from spamfilter";
}

sub assert_greylisting {
  my $longname = $_[0];

  my $toaddr;
  my $fromaddr = "test\@test.test";
  my @chars = ("A".."Z", "a".."z");
  $toaddr .= $chars[rand @chars] for 1..16;
  $toaddr .= "\@$domain";

  my ($status, $description, $data) = expect_smtp $longname, $fromaddr, $toaddr;
  if ($status ~~ [3, 4]) {
    good "Greylisting: Greylisted $fromaddr -> $toaddr";
  } else {
    bad "Greylisting: Did not work properly: $description\n'$data'";
  }
}

sub assert_no_greylisting {
  my $longname = $_[0];

  my $toaddr;
  my $fromaddr = "test\@test.test";
  my @chars = ("A".."Z", "a".."z");
  $toaddr .= $chars[rand @chars] for 1..16;
  $toaddr .= "\@$domain";

  my ($status, $description, $data) = expect_smtp $longname, $fromaddr, $toaddr;
  if ($status == 6) {
    good "Greylisting: Not greylisted $fromaddr -> $toaddr";
  } else {
    bad "Greylisting: Greylisted: $description\n'$data'";
  }
}

sub assert_ssh_login {
  my $shortname = $_[0];
  my $user = $_[1];
  my $passwd = $_[2];
  my $ssh_arg = $_[3] ? $_[3] : "";

  chomp(my $tmp_name = qx{mktemp});
  open my $tmp_file, '>', $tmp_name or die $!;
  print $tmp_file qq|
set timeout 7
spawn ssh $ssh_arg $user\@$shortname
expect {
  "yes/no" {
    send "yes\r"
    exp_continue
  }

  "password:" {
    send "$passwd\r"
    exp_continue
  }

  "Linux $shortname 2.6.32 #2 Fri May 27 16:03:41 CEST 2011 i686" {
    exit 0
  }

  "Permission denied (publickey,password)." {
    exit 3
  }

  timeout {
    exit 2
  }
}

exit 1
|;
  close $tmp_file;
  qx{expect <$tmp_name};
  #system "expect <$tmp_name";
  if ($? == 0) {
    good "Can login as $user on $shortname";
  } else {
    my $description;
    switch ($? >> 8) {
      case 1 { $description = "Unexpected reply from server"; }
      case 2 { $description = "Connection timed out"; }
      case 3 { $description = "Permission denied"; }
      else   { $description = "Unknown error ($? >> 8)"; }
    }
    bad "Failed to login as $user on $shortname ($description)";
  }
  unlink $tmp_name;
}

sub assert_showmount {
  my $host = $_[0];
  my $data = $_[1];
  qx[showmount -e $host | grep \"^$data\$\"];
  if ($? == 0) {
    good "Can view '$data' on $host";
  } else {
    bad "Cannot find '$data' on $host";
  }
}

sub assert_mounted_usr_local {
  qx{mount | grep "130.236.179.90:/usr/local on /usr/local"};
  if ($? == 0) {
    good "/usr/local is mounted";
  } else {
    bad "/usr/local is NOT mounted";
  }
}

sub test_port_open {
  my $host = $_[0];
  my $port = $_[1];
  my $expected = $_[2];

  qx{nc -z $host $port};
  my $real = $? == 0;

  if ($real == $expected) {
    good "Port $port on $host open? " . ($real ? "Yes" : "No");
  } else {
    bad "Port $port on $host open? " . ($real ? "Yes" : "No");
  }
}

sub mail_to_gmail {
  qx{echo . | mail -r "olle<olle\@d4.sysinst.ida.liu.se>" -s "Test $hostname" me\@gmail.com};
  print qq{Make sure
  1. There is an email from each host
  2. All emails are from olle.surname\@d4.sysinst.ida.liu.se
  3. All emails should have Received-SPF: pass in header\n};
}


sub test_NET {
  $running_test = "NET";
  title "Making sure interfaces are configured correctly";
  switch ($hostname) {
    case "gw" {
      assert_if_config "lo",   "inet 127.0.0.1/8";
      assert_if_config "eth0", "inet 130.236.179.89/29 brd 130.236.179.95";
      assert_if_config "eth1", "inet 130.236.178.33/26 brd 130.236.178.63";
    }
    case "server" {
      assert_if_config "lo",   "inet 127.0.0.1/8";
      assert_if_config "eth0", "inet 130.236.179.90/29 brd 130.236.179.95";
    }
    case "client-1" {
      assert_if_config "lo",   "inet 127.0.0.1/8";
      assert_if_config "eth0", "inet 130.236.179.91/29 brd 130.236.179.95";
    }
    case "client-2" {
      assert_if_config "lo",   "inet 127.0.0.1/8";
      assert_if_config "eth0", "inet 130.236.179.92/29 brd 130.236.179.95";
    }
    else {
      bad "Unknown hostname";
    }
  }

  title "Making sure we can ping LAN";
  foreach my $client (@clients) {
    assert_ping $client->{ip};
    assert_ping $client->{longname};
    assert_ping $client->{shortname};
  }

  title "Making sure we can ping WAN";
  assert_ping "130.236.181.80";
  assert_ping "marsix.ida.liu.se";
  assert_ping "216.58.209.142";
  assert_ping "google.com";
}

sub test_DNS_LAN {
  $running_test = "DNS";
  title "Making sure we can look up domain";
  dns_lookup_SOA $domain, "$server{longname}.";
  dns_lookup_MX $domain, "10 $server{longname}.";

  title "Making sure we can look up LAN";
  foreach my $client (@clients) {
    dns_lookup_A $client->{longname}, $client->{ip};
  }

  title "Making sure we can reverse look up LAN";
  foreach my $client (@clients) {
    dns_lookup_reverse $client->{ip}, "$client->{rev_ip}\n$client->{longname}";
  }
}

sub test_DNS_WAN {
  $running_test = "DNS";
  title "Making sure we can look up WAN";
  dns_lookup_A "marsix.ida.liu.se", "130.236.181.80";
  dns_lookup_reverse "130.236.181.80", "marsix.ida.liu.se";
  dns_lookup_A "www.liu.se", "130.236.5.66";
  dns_lookup_reverse "130.236.5.66", "www.liu.se";
  dns_lookup_A "web1.bahnhof.se", "213.80.98.5";
  dns_lookup_reverse "213.80.98.5", "www.bahnhof.se";
}

sub test_NTP {
  $running_test = "NTP";
  title "Making sure we have the correct time server";
  assert_time;
  assert_num_timeservers;
  assert_timeserver;
}

sub test_LDAP {
  $running_test = "LDAP";
  title "Making sure LDAP works";
  my $testuser="test$hostname";
  my $testpass="testpasswd123";
  my $testpasssha="{SHA}4o7eajifFiz1ehmF+qmDZKiatys=";

  ldap_assert_exists $testuser, 0;
  ldap_add $testuser, $testpasssha;
  ldap_assert_exists $testuser, 1;

  foreach my $client (@clients) {
    assert_ssh_login $client->{shortname}, $testuser, $testpass;
  }

  ldap_delete $testuser;
  ldap_assert_exists $testuser, 0;
}

sub test_NFS {
  $running_test = "NFS";
  title "Making sure NFS works";
  if ($hostname ~~ ['server', 'client-1', 'client-2']) {
    assert_showmount $server{shortname}, "/usr/local $network_id";
    assert_showmount $server{shortname}, "/home1     $network_id";
    assert_showmount $server{shortname}, "/home2     $network_id";
  }

  if ($hostname ~~ ['client-1', 'client-2']) {
    assert_mounted_usr_local;
    assert_ssh_login $hostname, "ollehome1", "", "-i id_rsa_ollehome";
    assert_ssh_login $hostname, "ollehome2", "", "-i id_rsa_ollehome";
  }
}

sub test_EMB_WAN {
  $running_test = "EMB";

  # Delete all email for user olle
  qx{echo "d *" | ssh -i id_rsa_d4 root\@server.d4.sysinst.ida.liu.se mail -u olle -f ~olle/Maildir};

  title "Making sure SMTP port is only open on server";
  foreach my $client (@clients) {
    my $port_smtp = "25";
    my $should_smtp_be_open = 0;
    if ($client->{longname} eq $server{longname}) {
      $should_smtp_be_open = 1;
    }
    test_port_open $client->{longname}, $port_smtp, $should_smtp_be_open;
  }

  title "Making sure $server{longname} can receive email";
  assert_smtp_send $server{longname}, "test\@test.test", "olle\@$domain";
  assert_smtp_send $server{longname}, "test\@test.test", "olle.surname\@$domain";

  title "Making sure imap works";
  assert_imap_login $server{longname}, "root", $ldap_password;
  assert_greylisting $server{longname};

  title "Making sure we can not relay to other hosts";
  assert_no_relay $server{longname};

  title "Making sure spamfilter works";
  assert_smtp_spamfilter $server{longname}, "test\@test.test", "olle\@$domain";

  title "Making sure SPF works";
  dns_lookup_TXT $domain, "\"v=spf1 mx -all\"";

  title "Sending email to my gmail";
  mail_to_gmail;
}

sub test_EMB_LAN {
  $running_test = "EMB";

  title "Making sure $server{longname} can receive email";
  assert_smtp_send $server{longname}, "test\@test.test", "olle\@$domain";
  assert_smtp_send $server{longname}, "test\@test.test", "olle.surname\@$domain";

  title "Making sure imap works";
  assert_imap_login $server{longname}, "root", $ldap_password;
  assert_no_greylisting $server{longname};

  title "Sending email to my gmail";
  mail_to_gmail;
}

unless (caller) {
  my $test = $ARGV[1];
  $running_test = "ALL";

  ## Basic configuration check:
  title "Making sure we have the correct host";
  assert "Shortname", qx{hostname -s}, "$ARGV[0]";
  assert "Longname",  qx{hostname -f}, "$ARGV[0].$domain";

  test_NET     if ($test ~~ ['ALL', 'NET']);
  test_DNS_LAN if ($test ~~ ['ALL', 'DNS']);
  test_DNS_WAN if ($test ~~ ['ALL', 'DNS']);
  test_NTP     if ($test ~~ ['ALL', 'NTP']);
  test_LDAP    if ($test ~~ ['ALL', 'LDAP']);
  test_NFS     if ($test ~~ ['ALL', 'NFS']);
  test_EMB_LAN if ($test ~~ ['ALL', 'EMB']);

  print "Tests done on '$ARGV[0]'\n";
  exit 0;
}

