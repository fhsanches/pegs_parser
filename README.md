# pegs_parser
A parser generator for Parsing Expression Grammars in Common Lisp

Known bugs:
- Cannot parse ranges and classes (e.g.: [a-z]):
- Prints values instead of returning them (and the printed info is not particularly useful).