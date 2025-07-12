.PHONY: build data include src

CXX ?= g++
LD ?= ld

build:
	$(CXX) src/main.cpp -o build/test -Iinclude/rapidfuzz-cpp -Iinclude/json/include
	./build/test


emacs-onetime:
	echo '(load-file (expand-file-name "~/.emacs.d/betterthai.el"))' >> ~/.emacs.d/init.el

emacs:
	cat src/betterthai.el > ~/.emacs.d/betterthai.el

