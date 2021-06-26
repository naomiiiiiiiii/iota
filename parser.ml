include Base
include Core
include Core_kernel
include String_lib
include Scanner
module MyString = String_lib
let o = Fn.compose

let () = Printf.printf "oiueeiuroeuo %s \n" "ooo"

module type PARSE = sig
  (*if a precondition is not met, raise syntax error*)
  exception SyntaxError of string
  type token

  (*PRE: L = (Id s)::rem, ie L starts with identifier token
    POST: id L = (s, rem)*)
  val id: token list -> string * token list

  (*PRE: L = (Key k)::rem, ie L starts with a particular keyword token
    POST: key k L = (k, rem)*)
  val key : string -> token list -> string * token list

(*PRE: L = (Nat n)::rem, ie L starts with nat token
    POST: natp L = (n, rem)*)
  val natp: token list -> int * token list

  (*PRE: L = Star::rem, ie L starts with unit token
    POST: natp L = ((), rem)*)
  val starp: token list -> unit * token list

  (*POST: epsilon L = ([], L)*)
  val epsilon : token list -> 'b list * token list

  (*POST: join f g a tries f a. if this fails, it returns g a.*)
  val (|:|) : (token list -> 'b) * (token list -> 'b) -> token list -> 'b

  (*POST: force f tries f a. if this fails, it forces toplevel failure.*)
  val force : (token list -> 'b * token list ) -> token list -> 'b * token list

  (* PRE: f a = (out1, rem1), g rem1 = (out2, rem2)
POST: circ g f applies f and g in sequence. that is,
circ g f a = ((out1, out2), rem2)*)
  val circ : (token list -> 'd * token list) -> (token list -> 'b * token list)
    -> token list -> ('b * 'd) * 'token list

  (* PRE: g rem = (out2, rem')
POST: keycircl g k applies (key k) and g in sequence. that is,
keycircl g k (Key k::rem) = (out2, rem')*)
  val keycircl : (token list -> 'a * token list) -> string -> token list ->
    'a * token list

  (* PRE: g L = (out1, Key k :: rem')
POST: keycircr k g applies g and (key k) in sequence. that is,
keycircr g k L = (out1, rem')*)
  val keycircr : string -> (token list -> 'a * token list) -> token list ->
    'a * token list

  (*POST: pipe f g pipes (the first component) of the output of f into g.
ie, pipe f g a = ((g (f a).1), (f a).2)*)
  val (>>): (token list -> 'b * token list) * ('b -> 'd) ->
    token list -> 'd * token list

  (*POST: repeat f start L = (L, a) where
L = [(f start).1, (f (f start).2).1, ...]
f a failed*)
  val repeat : (token list -> 'b * token list) -> token list -> 'b list * token list

  (*POST: reader p s will scan s into a token list, then give the token list
  to p. if p parses all the tokens into value a: 'a, a is returned.
  otherwise, reader fails.*)
 val reader: (token list -> 'a * token list) -> string -> 'a
end

module Parsing (Lex: LEXICAL): PARSE = struct
  type token = Lex.token
  exception SyntaxErr of string

  let id L = match L with
      (Lex.Id s :: toks) -> (a, toks)
    | _ -> raise SyntaxErr "expected identifier\n"

  let 



(*am already making an abstract syntax tree, making a parse tree over
here will just move over all that work to here*)
