#include <stdio.h>
#include <stdlib.h>

void fib(int rot) {

  int old_value = 1;
  int new_value = 0;
  int tmp;

  while (rot > 0) {
    tmp = old_value;
    old_value = new_value;
    new_value = tmp + old_value;
    rot -= 1;
  }
  printf("%d", new_value);
}

int main(int argc, char* argv[]) {

  int i;

  /* If two args, return fib(argv[2]) */
  if (argc == 2) {
    fib(atoi(argv[1]));
    printf("\n");
  }
  /* Else: run 20 fibs: */
  else {
    for (i = 0; i < 10; i++) {
      fib(i);
      printf("\t");
      fib(i + 10);
      printf("\n");
    }
  }
    
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
