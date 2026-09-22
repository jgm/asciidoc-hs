# Revision history for asciidoc-hs

## 0.1.1 -- 2026-08-27

  * Fix counter type for lowercase alpha start values.
    `{counter:name:a}` produced UpperAlphaCounter due to a copy-paste error.

  * Fix Meta Semigroup to keep title attributes with the title.
    Both branches of the docTitleAttributes case returned m2's attributes,
    so concatenating documents paired the first document's title with the
    second document's title attributes.

  * Allow empty cells in CSV/TSV tables.

  * Require ';' to terminate character entity references.

  * Traverse both components of definition list items in generic traversals.

  * Require a space or end of line after definition list markers.

  * Fix infinite loop on unterminated fenced and literal delimited blocks
    at end of document.

  * Consume extra backticks of a longer closing fence.
    A closing fence longer than the opening one (e.g. closing a ``` block
    with ````) was only matched up to the opening length, leaking the
    remaining backticks into the following block.

  * Guard against include cycles. A file that (transitively) included
    itself caused infinite recursion in `handleIncludes`.

  * Resolve attribute references at the point of use.
    Attribute references were only substituted in a post-processing pass
    using the end-of-document attribute values, so a reference before a
    redefinition incorrectly picked up the later value.

  * Infer implicit table headers from layout like Asciidoctor.
    Tables were given a header row by default unless the noheader option
    was set.  Asciidoctor instead only implies a header when the first row
    sits on a single line directly after the opening border and is
    followed by a blank line.

  * Add a benchmark suite.

  * Consume whole letter runs in the inline parser.

  * Try macro, autolink and email starts once per letter run.

  * Avoid the full cell-separator lookahead at every PSV cell character.

  * Speed up the inline, table and post-processing paths.

  * Flatten the parser monad stack. Replace the derived
    ReaderT/StateT-over-attoparsec stack with a hand-rolled, inlined
    equivalent, so primitive operations no longer pay for two layers
    of transformer binds. Backtracking semantics are unchanged: state
    changes made by a failed branch of `<|>` are discarded.

  * Reject non-definition-list lines with a substring check.

  * Skip typographic replacement when no trigger is present.

  * Scan plain inline text in chunks, not per word.

  * Replace attoparsec with a hand-rolled CPS parser.

  * Do typographic replacement in one pass, without a String round trip.

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

