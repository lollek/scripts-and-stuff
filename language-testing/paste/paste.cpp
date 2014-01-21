#include <iostream>
#include <fstream>
#include <string>

#include <getopt.h>

int usage(char **argv)
{
  std::cerr << "Usage: " << argv[0] <<
               " file1 file2 [-d DELIM]" << std::endl;
  return 1;
}

int parse(std::string line1, std::string line2, std::string delim)
{
  std::ifstream file1(line1.c_str());
  std::ifstream file2(line2.c_str());

  while (file1.good() || file2.good())
  {
    if (file1.good())
    {
      std::getline(file1, line1);
      std::cout << line1;
    }
    std::cout << delim;
    if (file2.good())
    {
      std::getline(file2, line2);
      std::cout << line2;
    }
    std::cout << std::endl;
  }

  file1.close();
  file2.close();

  return 0;
}

int main(int argc, char **argv)
{
  std::string filename1, filename2, delim = " ";

  /* Check optional argument */
  int c;
  while ((c = getopt(argc, argv, "d:")) != -1)
    switch(c)
    {
      case 'd': delim = optarg; break;
      default: return usage(argv);
    }

  /* Check for required arguments */
  if (argc - optind != 2)
    return usage(argv);
  filename1 = argv[optind];
  filename2 = argv[optind+1];

  return parse(filename1, filename2, delim);
}

/** TAIL INFO:
 * Name: Paste
 * Language: C++
 * State: Done
 * Compile: g++ -Wall -Wextra -pedantic -Werror -O3 paste.cpp -o paste
 *
 * This is an answer to a python-test question at my school, basically:
 * It should take two files as argument, and paste them with delim between.
 * It should also be possible to change the delimited with -d
 * Example: ./paste A.TXT B.TXT -d1
 */
