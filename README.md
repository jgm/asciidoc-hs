# asciidoc-hs

An [AsciiDoc](https://docs.asciidoctor.org/asciidoc/latest/) parser written in Haskell.

## Status

Mostly works, given my limited testing.

Not (yet) supported:

- [ ] Tables inside table cells
- [ ] Automatically assigned document attributes (e.g. `firstname`,
      derived from author)
- [ ] Conditional processing directives (`ifdef`, `ifndef`, etc.)

## Instructions

`cabal build` will build the library and an executable called `hasciidoc` that can
be used for testing.  `hasciidoc` will parse an input file and produce either a
Haskell representation or a JSON representation of the parsed AST. For help,
`hasciidoc --help`.

`cabal test` will run the tests.
