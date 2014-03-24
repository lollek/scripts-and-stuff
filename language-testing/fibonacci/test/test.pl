#! /usr/bin/env perl

use strict;
use warnings;

require 'fibonacci.pl';

use Test::Simple tests => 4;

ok(&fib(0) == 0);
ok(&fib(1) == 1);
ok(&fib(2) == 1);
ok(&fib(42) == 267914296);
