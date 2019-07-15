.PHONY: all build clean

start: 
	SHA1_SIG=$(SHA1_SIG) GITHUB_AGENT=$(GITHUB_AGENT) dune exec ./server.exe

build:
	dune build @install
	dune rules

all: build

install:
	dune install

rules:
	dune rules

uninstall:
	dune uninstall

clean:
	rm -rf _build
