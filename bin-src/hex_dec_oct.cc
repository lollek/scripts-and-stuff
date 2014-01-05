#include <cstring>
#include <string>
#include <iostream>
#include <iomanip>
#include <stdexcept>

/* Compile with std=c++11
 * rename to hex for hex output
 * rename to dec for dec output
 * rename to oct for oct output 
 * */

using namespace std;

int main(int argc, char *argv[])
{
  int base;

  /* Set base */
  if (!strcmp(argv[0], "hex") || !strcmp(argv[0], "./hex"))
  {
    cout << setbase(16);
    base = 16;
  }
  else if (!strcmp(argv[0], "dec") || !strcmp(argv[0], "./dec"))
  {
    cout << setbase(10);
    base = 10;
  }
  else if (!strcmp(argv[0], "oct") || !strcmp(argv[0], "./oct"))
  {
    cout << setbase(8);
    base = 8;
  }
  else
  {
    cerr << "Error! Please set progname to hex, dec or oct\n";
    return 1;
  }

  /* Print */
  for (int i = 1; i < argc; ++i)
  {
    if (base == 16)
      cout << "0x";
    else if (base == 8)
      cout << "0";
    try { cout << stoi(argv[i]); }
    catch (const invalid_argument &e) {}
  }

  cout << endl;
  return 0;
}
