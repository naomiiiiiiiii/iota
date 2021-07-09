Iota is a variant of the simply typed lambda calculus supporting a global memory store, general references, and the computation monad as primitives. 

The above implements an interpreter and REPL for Iota. It uses the build dune system. To run the REPL, install dune, then run

dune build interp.exe

_build/default/interp.exe 

from the directory containing the dune file

The grammar for Iota is:
![alt text]( https://github.com/misstaggart/monad_interpreter/blob/master/iota_grammar.png)

One of the most entertaining parts of this development was using parsing combinators. They aren’t related to my research, but I find them aesthetically pleasing. Look in source_parser.ml!

This work is inspired by LC Paulson’s *ML for the working programmer* and is done under the mentorship of Karl Crary.
