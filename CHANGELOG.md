# Revision history for asciidoc-hs

## 0.1.0.1 -- 2026-02-01

  * Fix character escaping issue (#3). Unconstrained forms of
    delimited constructions weren't being allowed after `++`.

  * Fix some footnote parsing issues (#2).

  * Fix parsing of document attributes in the body of the document (#1).
    Previously only those in the header were handled.

  * Change handling of doc attributes. Collect them in state so that
    we can handle attributes defined in the body of the document.

  * Friendlier error message than "endOfInput" on unexpected content
    at the end.

  * Move regression tests to test/regression.

## 0.1 -- 2025-11-30

  * Initial release.

