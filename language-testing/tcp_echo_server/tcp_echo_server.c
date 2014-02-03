#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>

#include <netdb.h>

int main(int argc, char *argv[]) {

  if (argc != 2) {
    fprintf(stderr, "Usage: ./tcp_echo_server port\n");
    return 1;
  }

  for (char *p = argv[1]; *p != '\0'; ++p) {
    if (!isdigit(*p)) {
      fprintf(stderr, "Error: port contains non-digits\n");
      return 1;
    }
  }

  struct addrinfo hints;
  memset(&hints, 0, sizeof(hints));
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_PASSIVE;

  struct addrinfo *results = NULL;
  int status = getaddrinfo(NULL, argv[1], &hints, &results);
  if (status != 0) {
    fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(status));
    return 1;
  }

  int sock = -1;
  for (struct addrinfo *p = results; p != NULL; p = p->ai_next) {
    sock = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
    if (sock == -1) {
      perror("socket");
      continue;
    }

    int one = 1;
    if (setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(one)) == -1) {
      perror("setsockopt");
      return 1;
    }

    if (bind(sock, p->ai_addr, p->ai_addrlen) == -1) {
      close(sock);
      perror("socket: bind");
      return 1;
    }

    break;
  }

  freeaddrinfo(results);
  if (sock == -1) {
    fprintf(stderr, "Failed to bind a socket\n");
    return 1;
  }

  if (listen(sock, 2) == -1) {
    perror("listen");
    return 1;
  }

  for (;;) {
    /* Accept new connections */
    int client = accept(sock, NULL, NULL);
    if (client == -1) {
      fprintf(stderr, "Failed to accept new connection\n");
      continue;
    }

    char buf[1024];
    int buflen = recv(client, buf, (sizeof(buf)/sizeof(buf[0])) -1, 0);
    if (buflen == -1) {
      fprintf(stderr, "Failed to receive data from client\n");
      close(client);
      continue;
    }

    buf[buflen] = '\0';

    if (send(client, buf, strlen(buf), 0) == -1) {
      fprintf(stderr, "Failed to send data to client");
    }

    close(client);
  }

  close(sock);
  return 0;
}
