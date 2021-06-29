include module type of Source_sig

module Lang: Source_sig.SOURCE

(*rename l s returns an s' such that s' \notin l*)
val rename : string list -> string -> string
(* PRE: M = \x1..xn.M0
   POST: stripabs M = ([x1 ... xn], M0), where M0 refers to x1 as Free(x1)
   ie, stripabs turns a lambda into a list of bound variables
   and its body*)
val stripAbs : Lang.exp -> string list * Lang.exp

(*val printer : exp -> unit*)

