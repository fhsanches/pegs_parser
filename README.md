# pegs_parser
A parser generator for Parsing Expression Grammars in Common Lisp (developed with SBCL).

PEGs are grammars that are free of ambiguity. They were created by Bryan Ford in a paper published in POPL'04.

Usage:
- Create a PEG file in the "pegs" folder, ex: "pegs/mypeg.peg" (check the example and Ford's paper for syntax);
- Load your Common Lisp REPL (I've tested with SBCL 1.4.16);
- Load the parser file with:

    (load "peg.lisp")
- Parse an expression "x=1" with:

  (with-peg-parse "mypeg" "x=1")
  
 -Alternatively, parse a file "file.txt" with:
 
  (with-peg-parse "mypeg" (file-string "file.txt"))

Known bugs:
- Cannot parse ranges and classes (e.g.: [a-z]);
- No error recovery;
- Lack of tests.
