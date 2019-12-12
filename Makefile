.PHONY: all build clean start default format

default: build

start:
	dune exec -- ./notabot.exe

build:
	dune build @install

all: build

format:
	dune build @fmt --auto-promote

clean:
	dune clean
