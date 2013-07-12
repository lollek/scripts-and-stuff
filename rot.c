#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

char wrap_around(char _min, char _curr, char _max) {
  while (_curr < _min)
    _curr = _max - (_min - _curr);
  while (_curr > _max)
    _curr = _min + (_curr - _max) - 1;
  return _curr;
}

void rotate(int rotations, char string[]) {
  char *newstring, *p, *q;

  newstring = malloc(strlen(string)+1);
  for (p = string, q = newstring; *p != '\0'; p++, q++) {
    if ((*p >= 'a') && (*p <= 'z'))
      *q = wrap_around('a', *p+rotations, 'z');
    else if ((*p >= 'A') && (*p <= 'Z'))
      *q = wrap_around('A', *p+rotations, 'Z');
    else
      *q = *p;
  }
  printf("%s\n", newstring);
  free(newstring);
}

void usage(char argv0[]) {
  printf("usage: %s <rot> <string>\n", argv0);
  printf("<rot> - how much to rotate, 1 rotates A->B\n");
  printf("<string> - string to rotate\n");
}

int main(int argc, char* argv[]) {
  if ((argc < 3) || (!isdigit(*argv[1])))
    usage(argv[0]);
  else
    rotate(atoi(argv[1]), argv[2]);

  return 0;
}
/* TAIL INFO:
Name: Rot
Language: C
Compile: cc rot.c -o rot_c -O3
State: Done w/ bugs

This application works like rot13 but expects user to name the amount of rotatations


Example: ./rot_c 13 "hello world"
*/
