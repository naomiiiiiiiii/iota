inspired by my research with Karl Crary and 'ML for the working programmer' by LC Paulson, I wrote an interpreter for our variant of the lambda calculus featuring global state and general references.

define the grammar so that it's clear function args have to be in parens

to understand parsing errors:
tokens are displayed as follows:
a keyword s := key~s
an identifier x := id~x
a numeral n := n
