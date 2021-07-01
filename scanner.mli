module type KEYWORD = sig
  val alpha_num : string list
  val symbols : string list
  val commentl: char
  val commentr: char
end

module type LEXICAL = sig
  type token = Id of string | Key of string | Nat of int
(*put unsigned int from here if you ever use it https://opam.ocaml.org/packages/stdint/*)
  val scan: string -> token list
val display_toks : token list -> string
end

module Lexical (Keywords: KEYWORD) : LEXICAL
