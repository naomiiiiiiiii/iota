Iota is a variant of the simply typed lambda calculus supporting a global memory store, general references, and the computation monad as primitives. 

This development implements an interpreter and REPL for Iota. It uses the dune build system. To run the REPL, install dune, then run

```
>> dune build interp.exe

>> _build/default/interp.exe
``` 

from the directory containing the dune file

Here are some example Iota programs:
- ![Imgur Image](https://imgur.com/nmBJdn0.png)
- ![Imgur Image](https://imgur.com/bLeFHIV.png)
- ![Imgur Image](https://imgur.com/QtF8oq0.png)


The grammar for Iota is:
![Imgur Image](https://imgur.com/h1Jt9nt.png)

One of the most entertaining parts of this development was using parsing combinators. They aren’t related to my research, but I find them aesthetically pleasing. Look in source_parser.ml!

This work is inspired by LC Paulson’s *ML for the working programmer* and is done under the mentorship of Karl Crary.
