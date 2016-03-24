import std.stdio;
import std.regex;
import std.conv;
import std.array;

string hodor(string line) {
  auto re = regex(r"\w+");
  return replaceAll(line, re, "hodor");
}

void main(string[] args) {
  if (args.length > 2) {
    writeln(hodor(args[1..args.length].join(" ")));
  } else {
    foreach (line; stdin.byLine) {
      writeln(hodor(to!string(line)));
    }
  }
}
