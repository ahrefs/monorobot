.PHONY: all build clean start default fmt test

default: build

start:
	dune exec -- ./src/monorobot.exe

build:
	dune build src/monorobot.exe

watch:
	dune build -w src/monorobot.exe

test:
	dune runtest

test_promote:
	dune runtest --auto-promote

all: build

fmt:
	dune build @fmt --auto-promote

clean:
	dune clean
