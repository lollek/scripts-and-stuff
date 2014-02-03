/* Compile with -std=c++11 */
#include <cctype>
#include <iostream>
#include <string>

std::string &rot13(std::string &line) {
  for (std::string::iterator c = line.begin(); c != line.end(); ++c) {
    if ('a' <= tolower(*c) && tolower(*c) <= 'm') {
      *c += 13;
    } else if ('n' <= tolower(*c) && tolower(*c) <= 'z') {
      *c -= 13;
    }
  }
  return line;
}

int main(int argc, char* argv[]) {
  /* If there is no argument, try to rot13 the stdin
   * This makes you able to pipe through the application */
  if (argc == 1) {
      std::string s;
      for (;;) {
        std::getline(std::cin, s);
        if (!std::cin) {
          break;
        } else {
          std::cout << rot13(s) << std::endl;
        }
      }
  /* Otherwise, rot13 the arguments */
  } else {
    for (int i = 1; i < argc; i++) {
      std::string s(argv[i]);
      std::cout << rot13(s) << " ";
    }
    std::cout << std::endl;
  }
  return 0;
}
