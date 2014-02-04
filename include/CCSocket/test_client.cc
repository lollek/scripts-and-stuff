#include <iostream>

#include "Socket.hh"

using namespace std;

int main(int argc, char *argv[]) {
  if (argc != 3) {
    cerr << "Usage: ./app hostname port\n";
    return 1;
  }

  Socket socket(IPV4, TCP);
  socket._connect(argv[1], stoi(argv[2]));
  string data;
  data.resize(1024);
  fgets(&data[0], 1024, stdin);
  socket._send(data);
  cout << socket._recv(1024);
  socket._close();

}