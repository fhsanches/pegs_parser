# pegs_parser
A parser generator for Parsing Expression Grammars in Common Lisp (developed with SBCL).

PEGs are grammars that are free of ambiguity. They were created by Bryan Ford in a paper published in POPL'04.

Usage:
- Create a PEG file in the "pegs" folder, ex: "pegs/mypeg.peg" (check the example and Ford's paper for syntax);
- Load your Common Lisp REPL (I've tested with SBCL 1.4.16);
- Load the parser file with (load "peg.lisp")
- (with-peg-parse "mypeg" "expression", to parse the expression using the mypeg grammar.

Known bugs:
- Cannot parse ranges and classes (e.g.: [a-z]):
- Prints values instead of returning them (and the printed info is not particularly useful).
