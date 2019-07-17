.PHONY: all build clean

start: 
	dune exec ./server.exe

build:
	dune build

all: build

install:
	dune install

rules:
	dune rules

format:
	dune build @fmt --auto-promote

uninstall:
	dune uninstall

clean:
	rm -rf _build
