.PHONY: build data include src

CXX ?= g++
LD ?= ld

build:
	$(CXX) src/main.cpp -o build/test -Iinclude/rapidfuzz-cpp -Iinclude/json/include
	./build/test