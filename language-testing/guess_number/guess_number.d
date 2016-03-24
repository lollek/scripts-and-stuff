import std.stdio;
import std.random;
import std.conv;

void main() {
  writeln("Guess-a-number game!\n",
          "I am thinking of a number between 1 and 100\n",
          "What is your guess?");

  int target = uniform(1, 101);
  int attempt = 0;
  immutable int max_attempts = 5;

  while (attempt < max_attempts) {
    attempt++;
    writef("Guess %d: ", attempt);

    int guess;
    try {
      readf(" %d", &guess);
    } catch (ConvException e) {
      writeln("That's not even a number!");
      attempt--;
      continue;
    }

    if (guess == target) {
      writeln("Correct! You won!");
      break;

    } else if (attempt == max_attempts) {
      writefln("Haha, I won! The number was %d", target);

    } else if (guess < target) {
      writeln("Too low! Guess again!");

    } else if (guess > target) {
      writeln("Too high! Guess again!");
    }
  }
}
