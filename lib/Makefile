CXX ?= g++
CXXFLAGS ?= -g -std=c++11 -I/home/linus/Projects/var/gog-galaxy/Include/

SRC=$(wildcard *.cpp)
OBJ=$(SRC:%.cpp=%.o)

all: $(OBJ)
	$(CXX) -o galaxy-c.dll $^ $(LDFLAGS)

%.o: %.c
	$(CXX) $(CXXFLAGS) $@ -c $<

clean:
	rm *.o
