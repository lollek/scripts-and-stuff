// Written in rust 0.9
// Jan 21 2014

use std::{str, os, io};
use std::io::buffered::BufferedReader;

fn rot_string(string: ~str) -> ~str {
  let bytestr: ~[u8] = string.into_bytes();
  str::from_utf8_owned(bytestr.iter().map(|&c| rot_char(c)).collect())
}

fn rot_char(c: u8) -> u8 {
  match c as char {
    'a' .. 'm' => c + 13,
    'n' .. 'z' => c - 13,
    'A' .. 'M' => c + 13,
    'N' .. 'Z' => c - 13,
    _          => c
  }
}

fn main() {
  let argv: ~[~str] = os::args();
  if argv.len() > 1 {
    let args: ~str = argv.slice_from(1).connect(" ");
    println(rot_string(args))
  } else {
    let mut stdin = BufferedReader::new(io::stdio::stdin());
    for line in stdin.lines() {
      print(rot_string(line))
    }
  }
}
