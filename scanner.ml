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
  type token = Id of string | Key of string | Nat of int
  val scan: string -> token list
val display_toks : token list -> string
end

module Lexical (Keywords: KEYWORD) : LEXICAL = struct
  type token = Id of string | Key of string | Nat of int

  (*tildes are just for legibility*)
  let display_tok t = match t with
      Id s -> " id~" ^ s
    | Key s -> " key~" ^ s
    | Nat n -> " nat~" ^string_of_int(n)

  let display_toks l = String.concat (List.map ~f:display_tok l)

(*PRE: a is a string comprised of alphanumeric characters
  POST: alphaTok a is the token resulting from scanning a as a keyword or identity*)
 let alphaTok : string -> token = fun a ->
   if (MyString.member Keywords.alpha_num a) then Key(a) else Id(a)

(*PRE: a is a string comprised of digits
  POST: natTok a is the token resulting from scanning a as a numeral*)
 let natTok a =
   try (let nat = int_of_string(a) in
                                             if (nat < 0) then
                                               (raise (Failure "Nats cannot be negative!"))
                                             else (Nat nat))
   with Failure error -> raise (Failure (error^" in NatTok")) (*catch exns from int_of_string*)

(*scan_symbol: string x string -> token x string
PRE: front nonempty, comprised of punctuation
POST: scan_symbol(front, rem) evaluates to (tok, rem') where tok is the first symbolic token in (front ++ rem) and rem' is remainder of rem left unscanned*)
  let rec scan_symbol(front, rem) =
    match (MyString.getc rem) with
      None -> (Key(front), rem)
    | Some(head, tail) ->
      if ((MyString.member Keywords.symbols front) || (not (MyString.isPunct(head))))
      then (Key(front), rem)
      else scan_symbol(front^String.of_char(head), tail)

(*scan_fn_help: token list x string -> token list
PRE: toks is list of tokens already scanned (read from right to left),
  s is string remaining to be scanned
POST: ouputs a list containing (the tokens from toks, then the tokens scanned from s)
  read from left to right *)
  let rec scan_help toks s =
    match (MyString.getc s) with
      None ->  List.rev toks
    | Some(head, tail) -> let (newtoks, news) =
      if (Char.equal head Keywords.commentl)
      then (toks, MyString.dropl_char tail Keywords.commentr "unclosed comment")
      else if Char.is_alpha(head) (*identifier or keyword*)
      then let (id, rem) = MyString.split_while Char.is_alphanum s in
        (alphaTok(id)::toks, rem)
      else if Char.is_digit(head) (*number*)
      then let (num, rem) = MyString.split_while Char.is_digit s in
        (natTok(num)::toks, rem)
      else if MyString.isPunct(head) (*special symbol*)
      then let (tok, rem)= scan_symbol(String.of_char(head), tail) in
        (tok::toks, rem)
      else (toks, (String.lstrip ~drop:(o not MyString.isGraph) s))
        in scan_help newtoks news
  let scan = scan_help []

end


