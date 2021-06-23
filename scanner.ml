module type LEXICAL = sig
  type token = Id of string | Key of string | Nat of int | Star
(*put unsigned int from here if you ever use it https://opam.ocaml.org/packages/stdint/*)
  val scan_fn: string -> token list
end

module type KEYWORD = sig
  val alpha_num : string list
  val symbols : string list
end

module Lexical (Keywords: KEYWORD) : LEXICAL = struct
  let type token = Id of string | Key of string | Nat of int | Star

(*PRE: a is a string comprised of alphanumeric characters
  POST: token resulting from scanning a*)
  let val alphaTok = fun a -> if member(a, Keywords.alpha_num) then Key(a)
        else Id(a)

(* PRE: scanned is a string comprised of punctuation
   POST: outputs a tuple (tok, rem') where tok is the first symbolic token in
   (scanned ++ rem) and rem' is remainder of rem left unscanned*)
let fun scan_symbol(scanned, rem)



(*Definition 1 : bool := true. doesnt work
Definition 1x : bool := true. doesnt work
Definition x1 : bool := true. works
 *)

(*types are
  nat: contains primitive values
  unit: contains primotive values
  arrow: keyword
  ref: keyword
  comp: keyword

a variable cannot be named a number
  it must be proceeded by a letter
  if an number is on its own then its a number *)
