let rec parse_term toks =
  ((circ term (*look for body of the lambda *)
      ((keycircr "."
          (keycircl
             (circ (repeat id) id) (*look for all the captured vars*)
             "\\") (*looking for a lambda*)
       ) >> cons)
   ) >> makeLambda) (*makelambda should ahve type string list x exp -> exp*)
  |:| ((circ (repeat atom) atom) >> applyList) (*single atom or application of atoms*)
  |:| ((keycircl atom "ret") >> makeRet) (*looking for a ret. make ret: exp -> exp*)
  |:| ((keycircl (keycircl (circ (keycircr ")" term) (*2nd term*)
                             (keycircr "," term)) (*1st term*)
               "(" )
         "bind") >> makeBind) (*looking for a bind. make bind : exp x exp -> exp*)
  |:| ((circ (keycircl term "in")
    (circ (keycircl (keycircl term "ref") "=")
  
    (keycircl id "let"))) >> makeRef) (*looking for a ref declaration
                                         makeref: ((string x exp) x exp) -> exp *)
  |:| ((circ term (keycircr ":=" id)) >> makeAssign) (*: (string * exp) -> exp*)
  |:| ((keycircl id "!") >> makeDeref) (*: string -> exp*)
  |:| (startp >> makeUnit) (*: unit -> exp*)


and let rec parse_atom toks =  (id >> free)
                               |:| ((keycircr ")" (keycircl term "(")) >> pi1)
                               |:| (natp >> nat)
