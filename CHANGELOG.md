# Revision history for asciidoc-hs

## 0.1.0.5 -- 2026-08-27

    Tables: skip whitespace before cell spec (#13).

## 0.1.0.4 -- 2026-08-11

  * Fix tracking of rowspans in table parsing (#13). Columns occupied
    by rowspans from previous rows were not skipped when recording the
    rowspans introduced by a new row, so tables with staggered rowspans
    could fail to parse or be parsed incorrectly.

  * Tables no longer have a footer by default (#13). Previously the
    last row of every table was treated as a footer unless the
    `nofooter` option was given. Now, as in Asciidoctor, a footer is
    only created when the `footer` option is given.

## 0.1.0.3 -- 2026-06-02

  * Open block delimiter is exactly two `--` (#12).
    Fixes  a performance bug with `---` thematic breaks.

  * Improve parsing of link descriptions (#6).

## 0.1.0.2 -- 2026-03-17

  * Allow fenced constructions to end with end-of-input (#9).

  * Fix issue in parsing bracketed arguments (#8).

  * Fix bug in table parsing with rowspans (#5).

  * Improved parsing of line and block comments (#4).

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

