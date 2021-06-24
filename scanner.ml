open Core


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
  type token = Id of string | Key of string | Nat of int | Star

(*PRE: a is a string comprised of alphanumeric characters
  POST: token resulting from scanning a*)
  (*let alphaTok : string -> token = fun a ->
    if member(a, Keywords.alpha_num) then Key(a) else Id(a)*)

(*TYPE: scan_symbol: string x char stream -> token x string stream
PRE: scanned is comprised of punctuation
POST: outputs a tuple (tok, rem') where tok is the first symbolic token in
   (scanned ++ rem) and rem' is remainder of rem left unscanned*)
  let scan_symbol(scanned, rem) =
    match (FStream.front rem) with
      Nil -> (Key(scanned), rem)
    | Some(head, tail) ->
      if (mem(scanned, Keywords.symbols) || is_alphanum(head))
      then (Key(scanned), rem)
      else scan_symbol(scanned^escaped(rem), tail)

  let scan_fn s = [Id ""]
end


(*use sets instead of lists
use streams
  Stream.of_string builds a char stream from a string <-
what you're really doing with those string functions is implementing a stream,
rephrase it in those terms by looking at the cmtool manual*)

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
