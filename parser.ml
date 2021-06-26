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
    POST: $k L = (k, rem)*)
  val key : string -> token list -> string * token list

(*PRE: L = (Nat n)::rem, ie L starts with nat token
    POST: natp L = (n, rem)*)
  val natp: token list -> int * token list

  (*PRE: L = Star::rem, ie L starts with unit token
    POST: natp L = ((), rem)*)
  val starp: token list -> unit * token list

  (*POST: epsilon L = ([], L)*)
  val epsilon : 'a -> 'b list * 'a

  (*POST: join f g a tries f a. if this fails, it returns g a.*)
  val join : ('a -> 'b) * ('a -> 'b) -> 'a -> 'b

  (*POST: force f tries f a. if this fails, it forces toplevel failure.*)
  val force : ('a -> 'b * 'c) -> 'a -> 'b * 'c

  (* PRE: f a = (out1, rem1), g rem1 = (out2, rem2)
POST: seq f g applies f and g in sequence. that is,
seq f g a = ((out1, out2), rem2)*)
  val circ : ('a -> 'b * 'c) * ('c -> 'd * 'e) -> 'a -> ('b * 'd) * 'e

  (* PRE: g rem = (out2, rem')
POST: keyseq k g applies (key k) and g in sequence. that is,
seq (k, g) (Key k::rem) = (out2, rem')*)
  val keycirc : string * (token list -> 'a * 'b) -> token list -> 'a * 'b

  (*POST: pipe f g pipes (the first component) of the output of f into g.
ie, pipe f g a = ((g (f a).1), (f a).2)*)
  val pipe: ('a -> 'b * 'c) * ('b -> 'd) -> 'a -> 'd * 'c

  (*POST: repeat f start L = (L, a) where
L = [(f start).1, (f (f start).2).1, ...]
f a failed*)
  val repeat : ('a -> 'b * 'a) -> 'a -> 'b list * 'a

  (*POST: reader p s will scan s into a token list, then give the token list
  to p. if p parses all the tokens into value a: 'a, a is returned.
  otherwise, reader fails.*)
 val reader: (token list -> 'a * 'b list) -> string -> 'a
end
