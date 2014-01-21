// Written in rust 0.9
// Jan 21 2014

use std::os::args;
use std::path::Path;
use std::io::fs::File;
use std::io::buffered::BufferedReader;

fn usage(arg0: &str) {
  println!("Usage: {} FILE1 FILE2 [-dDELIM]\n\
    FILE1 - file to the left\n\
    FILE2 - file to the right\n\
    -dDELIM - characted to separate FILE1 and FILE2", arg0)
}

fn fopen(filename: &str) -> BufferedReader<File> {
  let on_error = || fail!("Failed to open {:?}", filename);
  let path : Path = Path::new(filename);
  let file : File = File::open(&path).unwrap_or_else(on_error);
  BufferedReader::new(file)
}

fn parse_args(argv: &[~str]) {
  let mut file1 = ~"";
  let mut file2 = ~"";
  let mut delim = &"\t";

  for arg in argv.slice_from(1).iter() {
    if delim == &"\t" && arg.len() == 3 && arg.slice_to(2) == "-d" {
      delim = arg.slice_from(2);
    } else if file1 == ~"" {
      file1 = arg.to_owned();
    } else if file2 == ~"" {
      file2 = arg.to_owned();
    } else {
      return usage(argv[0]);
    }
  }

  paste(file1, file2, delim)
}

fn paste(filename1: &str, filename2: &str, delim: &str) {
  let mut reader1 = fopen(filename1);
  let mut reader2 = fopen(filename2);

  loop {
    let line1 = reader1.read_line().unwrap_or(~"");
    let line2 = reader2.read_line().unwrap_or(~"");

    if reader1.eof() && reader2.eof() { return }

    println!("{}{}{}", line1.trim_right(), delim, line2.trim_right());
  }
}

fn main() {
  let argv : ~[~str] = args();
  match argv.len() {
    3 => paste(argv[1], argv[2], &"\t"),
    4 => parse_args(argv),
    _ => usage(argv[0])
  }
}
