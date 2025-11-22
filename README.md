# asciidoc-hs

An [AsciiDoc](https://docs.asciidoctor.org/asciidoc/latest/) parser written in Haskell.

## Status

Mostly works, given my limited testing.

Not (yet) supported:

- [ ] Automatically assigned document attributes (e.g. `firstname`,
      derived from author)
- [ ] Sequence counters
- [ ] Conditional processing directives (`ifdef`, `ifndef`, etc.)

## Instructions

`cabal build` will build the library and an executable called `hasciidoc` that can
be used for testing.

`cabal test` will run the tests.
