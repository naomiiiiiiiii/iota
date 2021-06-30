
module type DISPLAY = sig
  type exp
  type typ


(*
these shouldnt be in the signature
rename l s returns an s' such that s' \notin l
val rename : string list -> string -> string
(* PRE: M = \x1..xn.M0
   POST: stripabs M = ([x1 ... xn], M0), where M0 refers to x1 as Free(x1)
   ie, stripabs turns a lambda into a list of bound variables
   and its body*)
  val stripAbs : Lang.exp -> string list * Lang.exp*)

(*prints to stdout*)
  val printer : exp -> unit
end


