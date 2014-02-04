#include <unistd.h>
#include <string.h>

#include <string>
#include <iostream>

#include <netdb.h>
#include <arpa/inet.h>

#include "Socket.hh"

using namespace std;

Socket::Socket(IPV ip_version, PROTO protocol) :
  ip_version_(ip_version),
  protocol_(protocol),
  sock_(-1),
  ip_(new char[INET6_ADDRSTRLEN]),
  reuseaddr_(false)
{
  memset(ip_, 0, INET6_ADDRSTRLEN);
}

Socket::Socket(const Socket &other) :
  ip_version_(other.ip_version_),
  protocol_(other.protocol_),
  sock_(other.sock_),
  ip_(new char[INET6_ADDRSTRLEN]),
  reuseaddr_(other.reuseaddr_)
{
  strncpy(ip_, other.ip_, INET6_ADDRSTRLEN);
  ip_[INET6_ADDRSTRLEN -1] = '\0';
}

Socket::~Socket() {
  delete[] ip_;
  ip_ = NULL;
}

void Socket::_reuseAddr(bool var) {
  reuseaddr_ = var;
}

Socket::Socket(int sock, const char *ip, IPV ip_version, PROTO protocol) :
  ip_version_(ip_version),
  protocol_(protocol),
  sock_(sock),
  ip_(new char[INET6_ADDRSTRLEN]),
  reuseaddr_(false)
{
  strncpy(ip_, ip, INET6_ADDRSTRLEN);
  ip_[INET6_ADDRSTRLEN -1] = '\0';
}

int Socket::construct(const char *hostname, const char *port) {
  struct addrinfo hints;
  memset(&hints, 0, sizeof(hints));
  hints.ai_family = ip_version_ == IPV4 ? AF_INET : AF_INET6;
  hints.ai_socktype = protocol_ == TCP ? SOCK_STREAM : SOCK_DGRAM;
  if (hostname == NULL) {
    hints.ai_flags = AI_PASSIVE;
  }

  struct addrinfo *results = NULL;
  int status = getaddrinfo(hostname, port, &hints, &results);
  if (status != 0) {
    cerr << "Error while looking up address info: "
         << gai_strerror(status) << '\n';
    return 1;
  }

  for (struct addrinfo *p = results; p != NULL; p = p->ai_next) {
    sock_ = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
    if (sock_ == -1) {
      perror("socket:");
      continue;
    }

    if (hostname == NULL) {
      if (reuseaddr_) {
        int one = 1;
        if (setsockopt(sock_, SOL_SOCKET, SO_REUSEADDR,
                       &one, sizeof(one)) == -1) {
          perror("setsockopt");
        }
      }

      if (bind(sock_, p->ai_addr, p->ai_addrlen) == -1) {
        close(sock_);
        perror("bind");
      } else {
        break;
      }

    } else {
      if (connect(sock_, p->ai_addr, p->ai_addrlen) == -1) {
        close(sock_);
        sock_ = -1;
      } else {
        break;
      }
    }
  }

  freeaddrinfo(results);

  if (sock_ == -1) {
    cerr << "Failed to bind\n";
    return 1;
  }
  return 0;
}

int Socket::_connect(const string &hostname, int port) {
  return construct(hostname.c_str(), to_string(port).c_str());
}

int Socket::_bind(int port) {
  return construct(NULL, to_string(port).c_str());
}

int Socket::_listen(int num) {
  if (listen(sock_, num) == -1) {
    perror("listen");
    return 1;
  }
  return 0;
}

Socket *Socket::_accept() {
  struct sockaddr_storage client_addr;
  socklen_t socklen = sizeof(client_addr);
  int client = accept(sock_, (struct sockaddr *)&client_addr, &socklen);
  if (client == -1) {
    perror("accept");
    return NULL;
  }

  void *sin_addr = NULL;
  if (((struct sockaddr *)&client_addr)->sa_family == AF_INET) {
    sin_addr = &(((struct sockaddr_in *)&client_addr)->sin_addr);
  } else {
    sin_addr = &(((struct sockaddr_in6 *)&client_addr)->sin6_addr);
  }
  char client_ip[INET6_ADDRSTRLEN];
  inet_ntop(client_addr.ss_family, sin_addr, client_ip, INET6_ADDRSTRLEN);

  return new Socket(client, client_ip, ip_version_, protocol_);
}

string Socket::_recvfrom(int num) {
  char *data = new char[num];
  struct sockaddr_storage client_addr;
  socklen_t socklen = sizeof(client_addr);
  char datalen = recvfrom(sock_, data, num -1,
                          (struct sockaddr *)&client_addr, &socklen);
  if (datalen == -1) {
    perror("recvfrom");
    return "";
  }

  data[datalen] = '\0';

  void *sin_addr = NULL;
  if (((struct sockaddr *)&client_addr)->sa_family == AF_INET) {
    sin_addr = &(((struct sockaddr_in *)&client_addr)->sin_addr);
  } else {
    sin_addr = &(((struct sockaddr_in6 *)&client_addr)->sin6_addr);
  }
  char client_ip[INET6_ADDRSTRLEN];
  inet_ntop(client_addr.ss_family, sin_addr, client_ip, INET6_ADDRSTRLEN);

  strncpy(ip_, client_ip, INET6_ADDRSTRLEN);
  string return_value(client_ip);
  delete[] data;
  return return_value;
}

string Socket::_recv(int num) {
  char *data = new char[num];
  int datalen = recv(sock_, data, num -1, 0);
  if (datalen == -1) {
    cerr << "Failed to read from client\n";
    return "";
  }

  data[datalen] = '\0';
  string return_value(data);
  delete[] data;
  return return_value;
}

void Socket::_send(const string &message) {
  const char *data = message.c_str();
  int data_left = strlen(data);
  while (data_left > 0) {
    int tmp = send(sock_, data +strlen(data) -data_left, data_left, 0);
    if (tmp == -1) {
      cerr << "Failed to send!\n";
      return;
    }
    data_left -= tmp;
  }
}

void Socket::_close() {
  close(sock_);
  delete[] ip_;
  ip_ = NULL;
}
