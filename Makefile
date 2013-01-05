o=dist

all: test

configure: $o/setup-config

$o/setup-config:
	@cabal configure --enable-tests

build: configure
	@cabal build

test: build
	@cabal test

install: test
	@cabal copy

docs:
	@cabal haddock --executables --hyperlink-source

clean:
	rm -rf $o

