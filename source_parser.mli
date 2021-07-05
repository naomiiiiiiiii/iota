
(*PRE: s is a declaration of the form 'let name = exp'
POST: read s returns (name, exp)*)
val read : string -> string * Source.exp
