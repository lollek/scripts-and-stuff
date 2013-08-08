#include <stdio.h>
#include <stdlib.h>

unsigned long fib(int rot) {

  unsigned long old_value = 1;
  unsigned long new_value = 0;
  unsigned long tmp;

  while (--rot >= 0) {
    tmp = old_value;
    old_value = new_value;
    new_value = tmp + old_value;
  }
  return new_value;
}

int main(int argc, char* argv[]) {

  int i;

  /* Fib the argument, if possible: */
  if (argc == 2)
    printf("%lu\n", fib(atoi(argv[1])));

  /* Else: run 20 fibs: */
  else
    for (i = 0; i < 10; i++)
      printf("%lu\t%lu\n", fib(i), fib(i+10));
    
  return 0;
}

/* TAIL INFO:
Name: Fibonacci Sequence
Language: C
Compile: cc fibonacci.c -o fibonacci_c
State: Done

Prints out numbers form the fibonacci sequence

Example: ./fibonacci_c
Example2: ./fibonacci_c 42
*/
