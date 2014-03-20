#include <iostream>
#include <cstdlib>

unsigned long fib (int i) {
  unsigned long oldval = 1;
  unsigned long newval = 0;
  unsigned long tmp;

  while (--i >= 0) {
    tmp = oldval;
    oldval = newval;
    newval = tmp + oldval;
  }
  return newval;
}

int main (int argc, char *argv[]) {

  // Fib the first argument, if possible
  if (argc == 2) {
    std::cout << fib(atoi(argv[1])) << std::endl;
    return 0;

  // Otherwise, run 20 fibs
  } else {
    for (int i = 0; i < 10; i++) {
      std::cout << fib(i) << "\t" << fib(i+10) << std::endl;
    }
  }
  return 0;
}
