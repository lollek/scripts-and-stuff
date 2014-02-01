#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>

#include <netdb.h>

int send_receive_data(const char *hostname, const char *port, const char *data)
{
  struct addrinfo hints, *results;
  memset(&hints, 0, sizeof(hints));
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_DGRAM;

  int status = getaddrinfo(hostname, port, &hints, &results);
  if (status != 0)
  {
    fprintf(stderr, "Error: %s\n", gai_strerror(status));
    return 1;
  }

  int sock = -1;
  struct addrinfo *p = NULL;
  for (p = results; p != NULL; p = p->ai_next)
  {
    sock = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
    if (sock != -1)
      break;
  }

  if (p == NULL)
  {
    fprintf(stderr, "Error: Could not connect to host\n");
    return 1;
  }

  if (sendto(sock, data, strlen(data), 0, p->ai_addr, p->ai_addrlen) == -1)
  {
    fprintf(stderr, "Error: Failed to send data\n");
    return 1;
  }

  char buf[1024];
  int buflen = recvfrom(sock, buf, (sizeof(buf)/sizeof(buf[0]))-1, 0,
                        NULL, NULL);
  if (buflen == -1)
  {
    fprintf(stderr, "Error: Failed to receive data\n");
    return 1;
  }

  if (buf[buflen -1] == '\n')
    buf[buflen -1] = '\0';
  else
    buf[buflen] = '\0';

  puts(buf);
  freeaddrinfo(results);
  close(sock);
  return 0;
}

int usage(const char *error)
{
  fprintf(stderr, "Usage: ./tcp_echo_client hostname port\nError: %s\n",
          error);
  return 1;
}

int main(int argc, char *argv[])
{
  if (argc == 1)
    return usage("No hostname provided!");
  else if (argc == 2)
    return usage("No port provided!");
  else if (argc > 3)
    return usage("Too many arguments!");
  for (char *c = argv[2]; *c != '\0'; ++c)
    if (!isdigit(*c))
      return usage("Port contains non-digits!");

  char contents[1024];
  if (fgets(contents, sizeof(contents)/sizeof(contents[0]), stdin) == NULL)
    return usage("No contents provided!");
  else
    return send_receive_data(argv[1], argv[2], &contents[0]);
}
