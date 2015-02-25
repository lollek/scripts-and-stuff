#include <iostream>
#include <random>

int main (void) {
  std::mt19937 engine{std::random_device{}()};
  std::uniform_int_distribution<int> rand{1, 100};
  int target{rand(engine)};

  std::cout <<
    "Guess-a-number game!\n"
    "I am thinking of a number between 1 and 100\n"
    "You have 5 tries to guess it correctly or I win\n"
    "What's your guess?\n";

  for (int attempt = 1; attempt < 6; ++attempt) {
    int current;

    std::cout << "Guess " << attempt << ": " << std::flush;
    std::cin >> current;

    if (current == target) {
      std::cout << "Correct! You won!" << std::endl;
      return 0;
    } else if (attempt == 5) {
      std::cout << "Haha, I won! The number was " << target << std::endl;
    } else if (current > target) {
      std::cout << "Too high! Try again!" << std::endl;
    } else if (current < target) {
      std::cout << "Too low! Try again!" << std::endl;
    }
  }
  return 0;
}
