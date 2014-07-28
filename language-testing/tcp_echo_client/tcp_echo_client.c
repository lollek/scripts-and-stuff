/* Compile with -std=gnu99 */
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include <unistd.h>
#include <netdb.h>

/* Header */
int send_receive_data(const char *hostname, const char *port, const char *data);
int usage(const char *error);


int send_receive_data(const char *hostname, const char *port,
                      const char *data) {
  struct addrinfo hints, *result;
  memset(&hints, 0, sizeof(hints));
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;

  int status = getaddrinfo(hostname, port, &hints, &result);
  if (status != 0) {
    fprintf(stderr, "Error: %s\n", gai_strerror(status));
    return 1;
  }

  int sock = -1;
  for (struct addrinfo *p = result; p != NULL; p = p->ai_next) {
    sock = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
    if (sock == -1) {
      continue;
    } else if (connect(sock, p->ai_addr, p->ai_addrlen) != -1) {
      break;
    } else {
      close(sock);
      sock = -1;
    }
  }

  freeaddrinfo(result);

  if (sock == -1) {
    fprintf(stderr, "Error: Failed to connect to host\n");
    return 1;
  }

  if (send(sock, data, strlen(data), 0) == -1) {
    fprintf(stderr, "Error: Failed to send data!\n");
    return 1;
  }

  char buf[256];
  int buflen = recv(sock, buf, (sizeof(buf) / sizeof(buf[0])) -1, 0);
  if (buflen == -1) {
    fprintf(stderr, "Error: Failed to receive data!\n");
    return 1;
  }

  if (buf[buflen -1] == '\n') {
    buf[buflen -1] = '\0';
  } else {
    buf[buflen] = '\0';
  }
  puts(buf);
  close(sock);

  return 0;
}

int usage(const char *error) {
  fprintf(stderr, "Usage: ./tcp_echo_client hostname port\nError: %s\n",
          error);
  return 1;
}

int main(int argc, char *argv[]) {
  const char *hostname = NULL;
  const char *port = NULL;
  char contents[256];

  for (int i = 1; i < argc; ++i) {
    if (hostname == NULL)
      hostname = argv[i];

    else if (port == NULL) {
      for (char *c = argv[i]; *c != '\0'; ++c) {
        if (!isdigit(*c)) {
          return usage("Port contains non-digits!");
        }
      }
      port = argv[i];
    }

    else {
      return usage("Too many arguments!");
    }
  }

  if (hostname == NULL) {
    return usage("No hostname provided!");
  } else if (port == NULL) {
    return usage("No port provided!");
  } else if (fgets(contents, sizeof(contents) / sizeof(contents[0]), 
             stdin) == NULL) {
    return usage("No contents provided!");
  } else {
    return send_receive_data(hostname, port, &contents[0]);
  }
}
