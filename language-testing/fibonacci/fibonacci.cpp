#include <iostream>
#include <cstdlib>
#include <string>
#include <assert.h>

using namespace std;

unsigned long fib (int rot) {
  unsigned long oldval = 1;
  unsigned long newval = 0;
  unsigned long tmp;

  for (int i = 0; i < rot; ++i) {
    tmp = oldval;
    oldval = newval;
    newval = tmp + oldval;
  }
  return newval;
}

int test() {
  cout << "Starting test cases\n";
  assert(fib(0) == 0);
  assert(fib(1) == 1);
  assert(fib(2) == 1);
  assert(fib(42) == 267914296);
  cout << "Success!" << endl;
  return 0;
}

int main (int argc, char *argv[]) {

  // Fib the first argument, if possible
  if (argc == 2) {
    if (!string(argv[1]).compare("test")) {
      return test();
    } else {
      cout << fib(atoi(argv[1])) << endl;
    }
  // Otherwise, run 20 fibs
  } else {
    for (int i = 0; i < 10; i++) {
      cout << fib(i) << "\t" << fib(i+10) << endl;
    }
  }
  return 0;
}
