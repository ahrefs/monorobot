.PHONY: all build clean start default format

default: build

start:
	dune exec -- ./src/notabot.exe

build:
	dune build

all: build

fmt:
	dune build @fmt --auto-promote

clean:
	dune clean
