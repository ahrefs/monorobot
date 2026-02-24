.PHONY: all build clean start default fmt test

ARGS ?=
.PHONY: dev
dev:
	watchexec -i _build -i _opam -w lib -w src -e ml,mli -r -c "dune build && dune exec monorobot -- $(ARGS)"

default: build

start:
	dune exec -- ./src/monorobot.exe

build:
	dune build src/monorobot.exe

watch:
	dune build -w src/monorobot.exe

release:
	dune build --profile=release src/monorobot.exe

test:
	dune runtest

test_promote:
	dune runtest --auto-promote

all: build

fmt:
	dune build @fmt --auto-promote

clean:
	dune clean
