#include <stdio.h>
#include <ctype.h>

/* Moves all [a-zA-Z] forward/backward 13 steps in the alphabet */
int rot13(int c) {
  if ('a' <= tolower(c) && tolower(c) <= 'z')
    return tolower(c)+13 <= 'z' ? c+13 : c-13;
  return c;
}

int main(int argc, char* argv[] ) {

  int i, c;
  char *p;

  /* If no argument: try to rot13 the stdin */
  if (argc < 2)
    while ((c = getc(stdin)) != EOF)
      putchar(rot13(c));
  return 0;

  /* Otherwise, rot13 the arguments */
  for (i = 1; i < argc; i++) {
    for (p = argv[i]; *p != '\0'; p++)
      putchar(rot13(*p));
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
