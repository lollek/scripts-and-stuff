
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main(void) {

  int target = rand() % 100 + 1;
  int current = 0;
  int attempt = 0;
  char buf[5];

  printf("Guess-a-number game!\n"
         "I am thinking of a number between 1 and 100\n"
         "You have 5 tries to guess it correctly or I win\n"
         "What's your guess?\n");
  fflush(stdout);

  while (target != current) {
    
    if (++attempt == 6) {
      printf("Haha, I won! The number was %d\n", target);
      return 0;
    }
    
    printf("Guess %d: ", attempt);
    fflush(stdout);
    fgets(buf, sizeof(buf), stdin);
    current = atoi(buf);

    if (current > target)
      printf("Too High! Try again!\n");
    else if (current < target)
      printf("Too Low! Try again!\n");
    else if (current == target) {
      printf("Correct! You won!\n");
      return 0;
    }
  }
  return 0;
}
/* TAIL INFO:
Name: Guess Number
Language: C
Compile: cc guess_number.c -o guess_number_c
State: Done

Play guess-a-number game


Example: ./guess_number_c
*/
