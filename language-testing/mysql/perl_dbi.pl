#! /usr/bin/env perl

use strict;
use warnings;
use DBI;

sub print_dbdata {
  # This subroutine fetches book info from the local MySQL database

  my $dbh = DBI->connect('DBI:mysql:mysql:localhost', "read_only")
    or die "connect: $DBI::errstr\n";
  my $sth = $dbh->prepare('select title, author, genre, comment from books' .
                          ' order by genre, author;')
    or die "prepare: $dbh->errstr\n";
  $sth->execute
    or die "execute: $sth->errstr\n";

  my $last_genre = "";
  while ((my $title, my $author, my $genre, my $comment)
         = $sth->fetchrow_array()) {
    if ($genre ne $last_genre) {
      print "</ol>" if $last_genre;
      print "<h3>$genre</h3>";
      print "<ol>";
      $last_genre = $genre;
    }
    print "<li><b>$title</b> av <b>$author</b>";
    print " ($comment)" if $comment;
    print "</li>";
  }
  print "</ol>";

  $dbh->disconnect
    or warn "disconnect: $dbh->errstr\n";
}

sub boilerplate {
  return <<EOF
<html>
  <head>
    <meta http-equiv="content-type" content="text/html; charset=utf-8" />
    <title>Title</title>
  </head>
  <body>
  <h1>Title</h1>
  <h2>Books</h2>
  <div id="books">
  </div>
  </body>
</html>
EOF
}

unless (caller) {
  # Change stdout to index.html
  open STDOUT, ">", "index.html"
    or die "open: $!\n";

  # Write a bit of boilerplate, then write dbdata,
  # then write the rest of the boilerplate

  foreach my $line (split(/\s*\n\s*/, &boilerplate)) {
    print $line;
    if ($line =~ /<div id="books">/) {
      &print_dbdata;
    }
  }
}
