
(*rename (l, s) returns an s' such that s' \notin l*)
val rename : string -> string list -> string
(* PRE: M = \x1..xn.M0
   POST: stripabs M = ([x1 ... xn], M0), where M0 refers to x1 as Free(x1)
   ie, stripabs turns a lambda into a list of bound variables
   and its body*)
val stripAbs : Source.exp -> string list * Source.t

val printer : Source.exp -> unit
