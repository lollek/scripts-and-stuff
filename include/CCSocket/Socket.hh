
#include <string>

enum PROTO { TCP, UDP };
enum IPV { IPV4, IPV6 };

class Socket {
  public:
    Socket (IPV ip_version, PROTO protocol);
    Socket (const Socket &other);
    ~Socket();

    void operator=(const Socket&) = delete;

    void _reuseAddr(bool var);
    int _connect(const std::string &hostname, int port);
    int _bind(int port);
    int _listen(int num);
    Socket *_accept();
    std::string _recv(int num);
    void _send(const std::string &message);
    void _close();

  private:
    Socket(int sock, const char *ip, IPV ip_version, PROTO protocol);
    int construct(const char *hostname, const char *port);

    const IPV ip_version_;
    const PROTO protocol_;
    int sock_;
    char *ip_;
    bool reuseaddr_;
};
