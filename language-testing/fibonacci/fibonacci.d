import std.stdio;
import std.conv;

ulong fib(ulong n) {
  ulong old_value = 1;
  ulong new_value = 0;

  foreach (_; 0..n) {
    immutable auto tmp = new_value;
    new_value += old_value;
    old_value = tmp;
  }

  return new_value;
}

void main(string[] args) {
  if (args.length > 1) {
    try {
      writeln(fib(to!ulong(args[1])));
    } catch (ConvException e) {
      writefln("%s: Failed to convert %s to digit", args[0], args[1]);
    }

  } else {
    foreach (i; 0..10) {
      writefln("%lu\t%lu", fib(i), fib(i+10));
    }
  }
}
