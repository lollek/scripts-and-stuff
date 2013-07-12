#include <stdio.h>

/* Moves all [a-zA-Z] forward/backward 13 steps in the alphabet */
void rot13(char c) {
  if ('a' <= c && c <= 'z')
    putchar(c+13 <= 'z'? c+13:c-13);
  else if ('A' <= c && c <= 'Z')
    putchar(c+13 <= 'Z'? c+13:c-13);
  else
    putchar(c);
}

int main( int argc, char* argv[] ) {

  int i;
  char c, *p;

  /* If no argument: try to rot13 the stdin: 
     This will make it possible to pipe it like this:
     > echo "Hello World" | ./rot13 
  */
  if (argc < 2)
    while ((c = getc(stdin)) != EOF)
      rot13(c);

  /* Otherwise, rot13 the arguments: 
     This will make it possible to:
     > ./rot13 Hello World 
  */
  for (i = 1; i < argc; i++) {
    for (p = argv[i]; *p != '\0'; p++)
      rot13(*p);
    putchar(' ');
  }
  putchar('\n');
  
  return 0;
}


/* TAIL INFO:
Name: ROT13
Language: C
Compile: cc rot13.c -o rot13_c -O3
State: Done

Rot13 a string

Example: ./rot13_c hello world
Example2: echo "Hello world" | ./rot13_c
*/
