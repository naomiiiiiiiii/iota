include Base
include Core
include Core_kernel
include String_lib
module MyString = String_lib
let o = Fn.compose

module type KEYWORD = sig
  val alpha_num : string list
  val symbols : string list
  val commentl: char
  val commentr: char
end

module type LEXICAL = sig
  type token = Id of string | Key of string | Nat of int | Star
(*put unsigned int from here if you ever use it https://opam.ocaml.org/packages/stdint/*)
  val scan_fn: string -> token list
end


module Lexical (Keywords: KEYWORD) : LEXICAL = struct
  type token = Id of string | Key of string | Nat of int | Star

(*PRE: a is a string comprised of alphanumeric characters
  POST: token resulting from scanning a*)
 let alphaTok : string -> token = fun a ->
   if (MyString.member Keywords.alpha_num a) then Key(a) else Id(a)

 let natTok: string -> token = fun a -> try (let nat = int_of_string(a) in
                                             if (nat < 0) then
                                               (raise (Failure "Nats cannot be negative!"))
                                             else (Nat nat))
   with Failure error -> raise (Failure (error^" in NatTok"))

(*TYPE: scan_symbol: string x string -> token x string
PRE: front nonempty, comprised of punctuation
POST: outputs a tuple (tok, rem') where tok is the first symbolic token in
   (front ++ rem) and rem' is remainder of rem left unscanned*)
  let rec scan_symbol(front, rem) =
    match (MyString.getc rem) with
      None -> (Key(front), rem)
    | Some(head, tail) ->
      if ((MyString.member Keywords.symbols front) || (not (MyString.isPunct(head))))
      then (Key(front), rem)
      else scan_symbol(front^String.of_char(head), tail)

(*TYPE: scan_fn_help: token list x string -> token list
PRE: toks is list of tokens already scanned (read from right to left),
  s is string remaining to be scanned
POST: ouputs a list containing (the tokens from toks, then the tokens scanned from s)
  read from left to right *)
  let rec scan_fn_help toks s =
    match (MyString.getc s) with
      None -> List.rev toks
    | Some(head, tail) -> let (newtoks, news) =
      if (Char.equal head Keywords.commentl)
      then (toks, MyString.dropl_char tail Keywords.commentr "unclosed comment")
      else if Char.is_alpha(head) (*identifier or keyword*)
      then let (id, rem) = MyString.partition (Char.is_alphanum) s in
        (alphaTok(id)::toks, rem)
      else if Char.is_digit(head) (*number*)
      then let (num, rem) = MyString.partition (Char.is_digit) s in
        (natTok(num)::toks, rem)
      else if MyString.isPunct(head) (*special symbol*)
      then let (tok, rem)= scan_symbol(String.of_char(head), tail) in
        (tok::toks, rem)
      else (toks, (String.lstrip ~drop:(o not MyString.isGraph) s))
        in scan_fn_help newtoks news
(*scan_symbol can do symbols of length > 1 and takes in a rem, come back here and fix comments*)
  let scan_fn = scan_fn_help []

end


(*use sets instead of lists https://ocaml.janestreet.com/ocaml-core/109.55.00/tmp/core_kernel/Set.html*)

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

(*use streams
  Stream.of_string builds a char stream from a string <-
what you're really doing with those string functions is implementing a stream,
rephrase it in those terms by looking at the cmtool manual*)
