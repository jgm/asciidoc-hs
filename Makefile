all: build test
.PHONY: all

build:
	cabal build
.PHONY: build

test:
	cabal test --test-options="--size-cutoff=10000 ${TESTARGS}"
.PHONY: test

linkbin:
	ln -s `cabal list-bin hasciidoc` hasciidoc
.PHONY: linkbin

binpath:
	@cabal list-bin hasciidoc
.PHONY: binpath

clean:
	cabal clean
.PHONY: clean
