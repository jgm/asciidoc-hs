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

check-cabal: git-files.txt sdist-files.txt
	@echo "Checking to see if all committed test files are in sdist."
	diff -u $^
	cabal check --ignore=missing-upper-bounds
	cabal outdated

.FORCE:

sdist-files.txt: .FORCE
	cabal sdist --list-only | sed 's/\.\///' | grep '^test/' | sort > $@

git-files.txt: .FORCE
	git ls-tree -r --name-only HEAD | grep '^test/' | sort > $@

