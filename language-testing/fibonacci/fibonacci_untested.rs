// Written in rust 0.9
// Jan 20 2014

fn fib(rot: int) -> int {
  let mut old_value = 1;
  let mut new_value = 0;
  for _ in range(0, rot) {
    let tmp = old_value;
    old_value = new_value;
    new_value += tmp;
  }
  new_value
}

fn main() {
  let arg: ~[~str] = std::os::args();
  if arg.len() > 1 {
    match from_str::<int>(arg[1]) {
      Some(x) => println!("{}", fib(x)),
      None    => println("Bad argument!")
    }
  } else {
    for r in range(0,10) {
      println!("{}\t{}", fib(r), fib(r+10))
    }
  }
}
