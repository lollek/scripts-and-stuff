#include <cctype>
#include <iostream>
#include <string>

void rot13(const char *string)
{
  bool a_to_m, n_to_z, too_high;

  for (const char *p = string; *p != '\0'; p++)
  {
    a_to_m = 'a' <= tolower(*p) ? true : false;
    n_to_z = 'n' <= tolower(*p) ? true : false;
    too_high = 'z' < tolower(*p) ? true : false;

    if (too_high) 
      std::cout << *p;
    else if (n_to_z)
      std::cout << static_cast<char> (*p - 13);
    else if (a_to_m)
      std::cout << static_cast<char> (*p + 13);
    else
      std::cout << *p;
  }
}

int main(int argc, char* argv[])
{
  /* If there is no argument, try to rot13 the stdin
   * This makes you able to pipe through the application */
  if (argc == 1)
    {
      std::string s;
      while (!std::cin.eof())
      {
        std::getline(std::cin, s);
        rot13(s.c_str());
        std::cout << std::endl;
      }
    }
  /* Otherwise, rot13 the arguments */
  else
  {
    for (int i = 1; i < argc; i++)
    {
      rot13(argv[i]);
      std::cout << " ";
    }
    std::cout << std::endl;
  }
  return 0;
}


/* TAIL INFO:
 * Name: Rot 13
 * Language: C++
 * Compile:  g++ -Wall -Wextra -pedantic -Werror -ansi -Weffc++ -O3 rot13.cpp -o rot13
 * State: Done
 *
 * Rot13 a string
 *
 * Example: ./rot13 hello world
 * Example2: cat rot13.cpp | ./rot13 
 * */
