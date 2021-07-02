include Base
include Core
include Core_kernel
include String_lib
module MyString = String_lib
include Scanner
include Source

open Parser

let cons (x, l) = x::l

(*shouldnt ever need to parse types,just print them from AST, going other way*)


module SourceKey : Scanner.KEYWORD = struct
  let alpha_num = ["ret"; "bind"; "let"; "ref"; "in"; "Nat"; "Unit"; "Ref"; "Comp"]
  and symbols = ["("; ")"; "\\"; "."; "="; ":="; "!"; "->"; "*"]
  and commentl = '['
  and commentr = ']'
end

module SourceLex : Scanner.LEXICAL = Scanner.Lexical(SourceKey)

module SourceParser : (Parser_sig.PARSER with type token = SourceLex.token) = Parser(SourceLex)


  open SourceParser

  let constant =  (natp >> Source.nat)
                   |:| (starp >> Source.star)

  let constant_typ = ((key "Nat") >> (fun _ -> Source.Nattp))
 |:| ((key "Unit") >> (fun _ -> Source.Unit))



  (*want this to be tok list -> type * tok list*)
  let rec typp toks = (((circ (keycircl atom_typp  "->") atom_typp) >> Source.arr)
    |:| ((keycircl atom_typp "Ref") >> Source.reftp)
    |:| ((keycircl atom_typp "Comp") >> Source.comp)
    |:| atom_typp) toks
                    (*start here automate the surrounded by parens thing,
                    shows up in 3 places*)
and atom_typp toks = (constant_typ
 |:|(keycircr ")" (keycircl typp "("))) toks
(*and atom_typp toks =
  let (s, rem) = (print_endline("running atom_typ on " ^ (SourceLex.display_toks toks));
((constant_typ
                     |:|(keycircr ")" (keycircl typp "("))) toks)) in
  print_endline("output tokens:"^(SourceLex.display_toks rem)); (s, rem)*)

let typed_id =keycircr ")" (circ typp (keycircr ":" (keycircl id "(")))

(*let typed_id toks =
print_endline ("in typed_id with "^ (SourceLex.display_toks toks)); 
let (s, rem) = keycircr ")" (circ typp (keycircr ":" (keycircl id "("))) toks in
  print_endline ("out of typed_id with" ^ (SourceLex.display_toks rem)); (s, rem)*)

let rec term toks =
(((circ term (*look for body of the lambda *)
      ((keycircr "."
          (keycircl 
             (circ (repeat typed_id) typed_id) (*look for all the captured vars with
                                               type annotations**)
             "\\") (*looking for a lambda*)
       ) >> cons) (*collects identifiers into a list*)
   ) >> Source.absList) (*turns list of identifiers and body into a lam*)
  |:| ((keycircl atom "!") >> Source.deref)
  |:| ((keycircl atom "ret") >> Source.ret) (*looking for a ret. make ret: exp -> exp*)
  |:| ((keycircl (keycircl (circ (keycircr ")" term) (*2nd term*)
                             (keycircr "," term)) (*1st term*)
               "(" )
         "bind") >> Source.bind) (*looking for a bind. make bind : exp x exp -> exp*)
  |:| ((keycircl atom "ref") >> Source.refexp) (*looking for a ref exp *)
  |:| ((circ term (keycircr ":=" atom)) >> Source.asgn) (*: (exp * exp) -> exp*)
 |:| ((circ (repeat atom) atom) >> Source.applyList) (*single atom or application of atoms
                                                          start here i dont think apply list should be in source*)
    ) toks
and atom toks = ((id >> Source.free)
                  |:| constant
                  |:| (keycircr ")" (keycircl term "("))) toks
                 
let read s = match term (SourceLex.scan s) with
    (m, []) -> m
  | (_, _::_) -> raise (SyntaxErr "Extra characters in phrase")
