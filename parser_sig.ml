(*various parser combinators*)

module type PARSER = sig
  type token

exception SyntaxErr of string
exception SyntaxErr_forced of string

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

  (*POST: (f |:| g) L tries f L. if this fails, it evaluates to g L.*)
  val (|:|) : (token list -> 'b) -> (token list -> 'b) -> token list -> 'b

  (*POST: force f L tries f L. if this fails, it forces the whole parser to fail.*)
  val force : (token list -> 'b * token list ) -> token list -> 'b * token list

  (* PRE: f a = (out1, rem1), g rem1 = (out2, rem2)
POST: circ g f does g composed with f. that is, circ g f a = ((out1, out2), rem2)*)
  val circ : (token list -> 'd * token list) -> (token list -> 'b * token list)
    -> token list -> ('b * 'd) * token list

  (* PRE: g rem = (out2, rem')
POST: keycircr g k does g o (key k), but it throws out k. that is, keycircr g k (Key k::rem) = (out2, rem')*)
  val keycircr : (token list -> 'a * token list) -> string -> token list ->
    'a * token list

  (* PRE: g L = (out1, Key k :: rem')
POST: keycircl k g does (key k) o g, but it throws out k. that is, keycircl g k L = (out1, rem')*)
  val keycircl : string -> (token list -> 'a * token list) -> token list ->
    'a * token list

  (*POST: p >> f pipes (the first component) of the output of p into f. ie, (p >> f) a = ((g (p a).1), (p a).2)
  *)
  val (>>): (token list -> 'b * token list) -> ('b -> 'd) ->
    token list -> 'd * token list

  (*POST: (repeat f start) applies f to start until f fails, recording the result of each application. that is, repeat f start = (L1, L2) where L1 has length n and
L1 = [(f start).1, (f (f start).2).1, ... ((f o (2 o f)^{n-1}) start).1]
L2 = ((f o (2 o f)^{n-1}) start).2
f L2 failed*)
  val repeat : (token list -> 'b * token list) -> token list -> 'b list * token list

  (*POST: reader p s will scan s into a token list, then give the token list
  to p. if p parses all the tokens into value a: 'a, a is returned.
  otherwise, reader fails.*)
 val reader: (token list -> 'a * token list) -> string -> 'a
end



