let rec parse_term toks =
  (circ term (*look for body of the lambda *)
    ((keycircr (keycircl (circ (repeat id) id) (*look for all the captured vars*)
    "\\") (*looking for a lambda*)
        ".") >> cons)) >> makeLambda
|:|

and let rec parse_atom toks =  (id >> free)
                               |:| ((keycircr ")" (keycircl term "(")) >> pi1)
                               |:| (natp >> nat)
