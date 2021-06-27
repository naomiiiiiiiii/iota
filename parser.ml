include Base
include Core
include Core_kernel
include String_lib
include Scanner
include Parser_sig
module MyString = String_lib
let o = Fn.compose
let () = Printf.printf "oiueeiuroeuo %s \n" "ooo"

module Parsing (Lex: LEXICAL): PARSE = struct
  type token = Lex.token
  exception SyntaxErr of string
  exception SyntaxErr_imeanitthistime of string

let cons (x, l) = x::l

  let id toks = match toks with
      (Lex.Id s :: rem) -> (s, rem)
    | _ -> raise (SyntaxErr "expected identifier\n")

  let key k toks = match toks with
      (Lex.Key k0 :: rem) -> if (String.equal k k0) then (k, rem)
      else raise (SyntaxErr ("expected keyword "^k^" got keyword "^k0^"\n"))
    | _ -> raise (SyntaxErr ("expected keyword "^k^"\n"))

  let natp toks = match toks with
      (Lex.Nat n::rem) -> (n, rem)
    | _  -> raise (SyntaxErr ("expected nat\n")) 

  let starp toks = match toks with
      (Lex.Star::rem) -> ((), rem)
    | _  -> raise (SyntaxErr ("expected unit\n")) 

  let epsilon toks = ([], toks)

  let (|:|) p1 p2 toks = try (p1 toks) with SyntaxErr _ ->
    (p2 toks)

  let force p = fun toks -> try (p toks) with SyntaxErr msg ->
    raise (SyntaxErr_imeanitthistime msg)

  let (>>) p f = fun toks -> let (x, rem) = (p toks) in
    (f x, rem)

  let circ p2 p1 = fun toks -> let (v1, toks1) = (p1 toks) in
    let (v2, toks2) = (p2 toks1) in
    ((v1, v2), toks2)

  let keycircl p k = (circ (force p) (key k)) >> snd

  let keycircr k p = (circ (key k) (force p)) >> fst

  (*if p decreases length of toks then tok is the decreasing argument
  and repeat p toks will terminate*)
  let rec repeat p toks = (((circ (repeat p) p) >> cons) |:| epsilon) toks

  let reader p s = match (p (Lex.scan_fn s)) with
      (e, []) -> e
    | _ -> raise (SyntaxErr ("Extra chars in phrase "^s^"\n"))
end


