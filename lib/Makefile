CXX ?= g++
CXXFLAGS ?= -fvisibility=hidden -fPIC -g -std=c++11 -I/home/linus/Projects/var/gog-galaxy/Include/
LDFLAGS ?= -lGalaxy64

SRC=$(wildcard *.cpp)
OBJ=$(SRC:%.cpp=%.o)

all: $(OBJ)
	$(CXX) -o galaxy-c.dll $^ $(LDFLAGS)

%.o: %.c
	$(CXX) $(CXXFLAGS) $@ -c $<

clean:
	rm *.o
