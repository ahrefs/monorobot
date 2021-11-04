.PHONY: all build clean start default fmt test

default: build

start:
	dune exec -- ./src/monorobot.exe

build: gen_version
	dune build src/monorobot.exe

gen_version:
	./gen_version.sh

test:
	dune runtest

test_promote:
	dune runtest --auto-promote

all: build

fmt:
	dune build @fmt --auto-promote

clean:
	rm -f src/version.ml
	dune clean
