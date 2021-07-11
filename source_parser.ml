include Base
include Core
include Core_kernel
include String_lib
module MyString = String_lib
include Scanner
include Source

open Parser

let cons (x, l) = x::l

(*definition of iota*)
module SourceKey : Scanner.KEYWORD = struct
  let alpha_num = ["ret"; "bind"; "let"; "ref"; "in"; "Nat"; "Unit"; "Ref"; "Comp"; "let"]
  and symbols = ["("; ")"; "\\"; "."; "="; ":="; "!"; "->"; "+"]
  and commentl = '['
  and commentr = ']'
end

module SourceLex : Scanner.LEXICAL = Scanner.Lexical(SourceKey)

(*parsing combinators for iota*)
module SourceParser : (Parser_sig.PARSER with type token = SourceLex.token) = Parser(SourceLex)

open SourceParser

(*parse constant value*)
let constant =  (natp >> Source.nat)
                |:| (starp >> Source.star)
(*parse base type*)
let constant_typ = ((key "Nat") >> (fun _ -> Source.Nattp))
                   |:| ((key "Unit") >> (fun _ -> Source.Unit))

(*parse a type*)
  let rec typp toks = (((circ (keycircr atom_typp  "->") atom_typp) >> Source.arr)
    |:| ((keycircr atom_typp "Ref") >> Source.reftp)
    |:| ((keycircr atom_typp "Comp") >> Source.comp)
    |:| atom_typp) toks
and atom_typp toks = (constant_typ
 |:|(keycircl ")" (keycircr typp "("))) toks

(*parse a type-annotated variable*)
let typed_id =keycircl ")" (circ typp (keycircl ":" (keycircr id "(")))

(*parse a term*)
let rec term toks =
(((circ term
      ((keycircl "."
          (keycircr 
             (circ (repeat typed_id) typed_id)
             "\\") (*looking for a lambda*)
       ) >> cons) (*collects bound identifiers into a list*)
   ) >> Source.absList) (*combines list of identifiers and body into a Lam*)
  |:| ((keycircr atom "!") >> Source.deref)
  |:| ((keycircr atom "ret") >> Source.ret)
  |:| ((keycircr (keycircr (circ (circ
                                    (keycircl ")" term) (*2nd term*)
                                    (keycircl "." (keycircr id "\\"))
                                 ) 
                             (keycircl "," term)) (*1st term*)
               "(" )
         "bind") >> Source.bind) 
  |:| ((keycircr atom "ref") >> Source.refexp) 
  |:| ((circ term (keycircl ":=" atom)) >> Source.asgn) 
  |:| (circ atom (keycircl "+" atom) >> Source.plus)
  |:| ((circ (repeat atom) atom) >> Source.applyList) (*single atom or application of atoms*)
    ) toks
and atom toks = ((id >> Source.free)
                  |:| constant
                  |:| (keycircl ")" (keycircr term "("))) toks
                 
let read s = 
  match s |> SourceLex.scan |> (circ term
                      (keycircl "=" (keycircr id "let"))) with
  (p, []) -> p
| (_, _::_) -> raise (SyntaxErr "Extra characters in phrase")
