# asciidoc-hs

An [AsciiDoc](https://docs.asciidoctor.org/asciidoc/latest/) parser written in Haskell.

The modern, asciidoctor form of the syntax is targeted, rather than the old legacy
syntax.

## Status

Mostly works, but testing has been limited.

Not (yet) supported:

- [ ] Tables inside table cells
- [ ] Automatically assigned document attributes (e.g. `firstname`,
      derived from author)
- [ ] Conditional processing directives (`ifdef`, `ifndef`, etc.)

## Installing

`cabal build` will build the library and an executable called `hasciidoc` that can
be used for testing.  `hasciidoc` will parse an input file and produce either a
Haskell representation or a JSON representation of the parsed AST. For help,
`hasciidoc --help`.

`cabal test` will run the tests.

## Using the library

For an example of the use of the library, see `app/Main.hs`.

`parseDocument` can be used in any instance of the Monad class.
The user must supply a function to retrieve the contents of an
included file and to raise an error. An AST is returned.
FromJSON instances are defined, so the AST may be rendered (and
read from) JSON.


