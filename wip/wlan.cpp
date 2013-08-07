#include <iostream>
#include <vector>
#include <string>
#include <cstdio>

/* Read subprocess stdin */
int read_process (std::string &output, const char* cmd)
{
  char buf[256];
  FILE *stream = NULL;

  if ((stream = popen(cmd, "r")) == NULL)
    return 1;
  while (fgets(buf, sizeof buf, stream))
    output.append(buf);
  pclose(stream);

  return output.empty();
}

int main (int argc, char *argv[]) 
{

  std::string s_known_aps;
  std::string s_nearby_aps;

  // Print header:
  std::cout << "\033[4mLollian WLAN Helper - 2013-08-01\033[0m\n\n";

  if(
    read_process(s_known_aps, "ls /root/wlan") != 0 
 || read_process(s_nearby_aps, "iwlist wlan0 scan") != 0
    ) return 1;
  

  std::cout << "Testing if I reach this\n";
  return 0;
}

/* TAIL INFO:
 * Name: Lollian Wlan Helper
 * Language: C++
 * Compile: g++ wlan.cpp -o wlan -Wall -Wextra -pedantic
 * State: Not Done

 * Find and connect to wireless access points
 * WPA Passwords should also be stored under /root/wlan

 * Example: ./wlan

 */
