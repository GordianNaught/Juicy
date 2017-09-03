# Juicy Language
## Syntax
The syntax of Juicy is currently C-like. This is not a firm choice, and the parser is implemented as a DCG (definite clause grammar). This makes the syntax simple to change through editing [juicy_parse.pl](/juicy_parse.pl). If you find even the tokenizing doesn't meet your needs, as may be the case for a newly introduced infix operator or literal data represenation, the tokenizer can be edited at [juicy_tokenize.pl](/juicy_tokenize.pl).

## Semantics
Juicy is not garbage collected and supports tail recursion. Currently the language is purely functional, but if a compelling reason to be otherwise is presented, it will be considered.
