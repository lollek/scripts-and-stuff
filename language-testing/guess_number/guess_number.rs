// Written in rust 0.9
// Jan 21 2014

use std::rand::random;
use std::io::stdio::{stdin, flush};
use std::io::buffered::BufferedReader;

fn get_rand() -> int {
  let solution = random::<int>() % 100 + 1;
  if solution < 0 { -solution }
  else            {  solution }
}

fn main() {
  let mut reader = BufferedReader::new(stdin());
  let solution = get_rand();
  let mut count = 0;

  println("Guess-a-number game!\n\
  I am thinking of a number between 1 and 100\n\
  You have 5 tries to guess it correctly or I win\n\
  What's your guess?");

  loop {
    count += 1;
    print!("Guess {}: ", count);
    flush();
    let strline = reader.read_line().unwrap_or(~"");
    match from_str::<int>(strline.trim()) {
      Some(num) => {
        if num == solution {
          println("Correct! You won!");
          return;
        } else if count == 5 {
          println!("Haha, I won! The number was {}", solution);
          return;
        } else if num < solution {
          println("Too low! Try again!");
        } else {
          println("Too high! Try again!");
        }
      },
      None => println("Hey, that's not a number!")
    }
  }
}
