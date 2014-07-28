#include <stdio.h>
#include <ctype.h>

/* Header */
int rot13(int c);


/* Moves all [a-zA-Z] forward/backward 13 steps in the alphabet */
int rot13(int c) {
  if ('a' <= tolower(c) && tolower(c) <= 'z') {
    return tolower(c)+13 <= 'z' ? c+13 : c-13;
  } else {
    return c;
  }
}

int main(int argc, char* argv[] ) {
  /* If no argument: try to rot13 the stdin */
  if (argc < 2) {
    int c;
    while ((c = getc(stdin)) != EOF) {
      putchar(rot13(c));
    }

  /* Otherwise, rot13 the arguments */
  } else {
    for (int i = 1; i < argc; i++) {
      for (char *p = argv[i]; *p != '\0'; p++) {
        putchar(rot13(*p));
      }
      putchar(' ');
    }
    putchar('\n');
  }
  return 0;
}
