#include <iostream>

#include "TCPSocket.hh"

using namespace std;

int main() {
  TCPSocket socket(IPV4);
  socket._reuseAddr(1);
  socket._bind(1337);
  socket._listen(10);

  for (;;) {
    TCPSocket *client = socket._accept();
    if (client == NULL) {
      return 1;
    }
    string data = client->_recv(1024);
    client->_send(data);
    client->_close();
    delete client;
    cout << data << '\n';
  }
  socket._close();
  return 0;
}
