#include <iostream>
#include <cstdlib>

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

#ifndef OBJECT_FILE
int main (int argc, char *argv[]) {

  // Fib the first argument, if possible
  // Otherwise, run 20 fibs
  if (argc == 2) {
    cout << fib(atoi(argv[1])) << endl;
  } else {
    for (int i = 0; i < 10; i++) {
      cout << fib(i) << "\t" << fib(i+10) << endl;
    }
  }
  return 0;
}
#endif
