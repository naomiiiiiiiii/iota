(*display interface for iota (the source language)*)

val typ_to_string: Source.typ -> string
val exp_to_string: Source.exp -> string

(*prints to stdout*)
val print_typ: Source.typ -> unit
val print_exp : Source.exp -> unit


