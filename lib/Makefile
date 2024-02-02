CXX ?= g++
CXXFLAGS ?= -g -std=c++11 -I/home/linus/Projects/var/gog-galaxy/Include/

SRC=$(wildcard *.cpp)
OBJ=$(SRC:%.cpp=%.o)

all: $(OBJ)
	$(CXX) $(LDFLAGS) -o galaxy-c.so $^

%.o: %.c
	$(CXX) $(CXXFLAGS) $@ -c $<

clean:
	rm *.o
