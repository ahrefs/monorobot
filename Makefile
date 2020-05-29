.PHONY: all build clean start default fmt test

default: build

start:
	dune exec -- ./src/notabot.exe

build:
	dune build

test:
	dune runtest

test_promote:
	dune runtest --auto-promote

all: build

fmt:
	dune build @fmt --auto-promote

clean:
	dune clean
