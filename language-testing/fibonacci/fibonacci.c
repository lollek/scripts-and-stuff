#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

unsigned long fib(int rot) {
  unsigned long old_value = 1;
  unsigned long new_value = 0;
  unsigned long tmp;
  int i;

  for (i = 0; i < rot; ++i) {
    tmp = old_value;
    old_value = new_value;
    new_value = tmp + old_value;
  }
  return new_value;
}

int test() {
  puts("Starting test cases");
  assert(fib(0) == 0);
  assert(fib(1) == 1);
  assert(fib(2) == 1);
  assert(fib(42) == 267914296);
  puts("Success!");
  return 0;
}

int main(int argc, char* argv[]) {
  int i;

  /* Fib the argument, if possible: */
  if (argc == 2) {
    if (!strcmp(argv[1], "test")) {
      return test();
    } else {
      printf("%lu\n", fib(atoi(argv[1])));
    }
  /* Else: run 20 fibs: */
  } else {
    for (i = 0; i < 10; ++i) {
      printf("%lu\t%lu\n", fib(i), fib(i+10));
    }
  }
  return 0;
}
