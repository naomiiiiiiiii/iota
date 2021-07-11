(*define your PL*)
module type KEYWORD = sig
  val alpha_num : string list
  val symbols : string list
  val commentl: char
  val commentr: char
end

module type LEXICAL = sig
  type token = Id of string | Key of string | Nat of int
  val scan: string -> token list
  val display_toks : token list -> string
end

(*scan a string into a list of tokens acccording to the keywords given in KEYWORDS*)
module Lexical (Keywords: KEYWORD) : LEXICAL
