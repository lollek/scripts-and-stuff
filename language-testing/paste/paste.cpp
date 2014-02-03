#include <iostream>
#include <fstream>
#include <string>

#include <getopt.h>

int usage(char **argv) {
  std::cerr << "Usage: " << argv[0]
            << " file1 file2 [-d DELIM]" << std::endl;
  return 1;
}

int parse(std::string line1, std::string line2, std::string delim) {
  std::ifstream file1(line1.c_str());
  std::ifstream file2(line2.c_str());

  for (;;) {
    if (file1) {
      std::getline(file1, line1);
    } else {
      line1 = "";
    }
    if (file2) {
      std::getline(file2, line2);
    } else {
      line2 = "";
    }
    if (file1 || file2) {
      std::cout << line1 << delim << line2 << std::endl;
    } else {
      break;
    }
  }

  file1.close();
  file2.close();

  return 0;
}

int main(int argc, char **argv) {
  std::string filename1, filename2, delim = " ";

  /* Check optional argument */
  int c;
  while ((c = getopt(argc, argv, "d:")) != -1) {
    switch(c) {
      case 'd': delim = optarg; break;
      default: return usage(argv);
    }
  }

  /* Check for required arguments */
  if (argc - optind != 2) {
    return usage(argv);
  }
  filename1 = argv[optind];
  filename2 = argv[optind+1];

  return parse(filename1, filename2, delim);
}
