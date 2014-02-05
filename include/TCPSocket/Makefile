CXX = g++
CXXFLAGS = -Wall -Wextra -Werror -pedantic -std=c++11 -Weffc++
OBJS = TCPSocket.o

all: $(OBJS)
	$(CXX) $(CXXFLAGS) -o example_client $(OBJS) example_client.cc
	$(CXX) $(CXXFLAGS) -o example_server $(OBJS) example_server.cc


