
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main(void) {

  int target = rand() % 100 + 1;
  int current = 0;
  char buf[5];

  printf("Guess-a-number game!\n"
         "I am thinking of a number between 1 and 100\n"
         "What's your guess? ");
  fflush(stdout);

  while (target != current) {
    
    fgets(buf, sizeof(buf), stdin);
    current = atoi(buf);

    if (current > target)
      printf("Too High! Try again: ");
    else if (current < target)
      printf("Too Low! Try again: ");
    else if (current == target) {
      printf("Correct! You won!\n");
      return 0;
    }
    fflush(stdout);

  }
  return 0;
}
/* TAIL INFO:
Name: Guess Number
Language: C
Compile: cc guess_number.c -o guess_number
State: Done

Play guess-a-number game


Example: ./guess_number
*/
