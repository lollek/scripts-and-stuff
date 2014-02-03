#include <iostream>
#include <cstdlib>

int main (void) {
  std::cout <<
    "Guess-a-number game!\n"
    "I am thinking of a number between 1 and 100\n"
    "You have 5 tries to guess it correctly or I win\n"
    "What's your guess?\n";

  int target = rand() % 100 + 1;
  int current = -1;
  int attempt = 1;

  while (target != current) {
    std::cout << "Guess " << attempt << ": " << std::flush;
    std::cin >> current;

    if (current == target) {
      std::cout << "Correct! You won!\n";
      return 0;
    } else if (++attempt == 6) {
      std::cout << "Haha, I won! The number was " << target << std::endl;
      return 0;
    } else if (current > target) {
      std::cout << "Too high! Try again!\n";
    } else if (current < target) {
      std::cout << "Too low! Try again!\n";
    }
  }
  return 0;
}
