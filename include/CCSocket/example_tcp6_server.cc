#include <iostream>

#include "Socket.hh"

using namespace std;

int main() {
  Socket socket(IPV6, TCP);
  socket._reuseAddr(1);
  socket._bind(1337);
  socket._listen(10);

  for (;;) {
    Socket *client = socket._accept();
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
