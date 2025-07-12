.PHONY: build data include src

CXX ?= g++
LD ?= ld


build:
	$(CXX) src/main.cpp -o build/test -Iinclude/rapidfuzz-cpp -Iinclude/json/include
	./build/test

emacs-init:
	mkdir -p ~/.emacs.d/betterthai-ime
	echo '(load-file (expand-file-name "~/.emacs.d/betterthai-ime/betterthai.el"))' >> ~/.emacs.d/init.el
	sudo cp -r data ~/.emacs.d/betterthai-ime/

emacs:
	sudo cp src/betterthai.el ~/.emacs.d/betterthai-ime/
	$(CXX) src/main-ime.cpp -o build/betterthai-ime -Iinclude/rapidfuzz-cpp -Iinclude/json/include
	sudo cp build/betterthai-ime ~/.emacs.d/betterthai-ime/
	sudo chmod +x ~/.emacs.d/betterthai-ime/betterthai-ime

