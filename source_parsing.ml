include Base
include Core
include Core_kernel
include String_lib
include Scanner
include Parser
include Parser_sig
include Source
module MyString = String_lib

let cons (x, l) = x::l
let o = Fn.compose

(*shouldnt ever need to parse types,just print them from AST, going other way*)

let () = Printf.printf "hurg \n"

module type PARSE_TERM = sig
  val read : string -> Source.exp end

module SourceKey : Scanner.KEYWORD = struct
  let alpha_num = ["ret"; "bind"; "let"; "ref"; "in"; "Nat"; "Unit"; "Ref"; "Comp"]
  and symbols = ["("; ")"; "\\"; "="; ":="; "!"; "->"]
  and commentl = '['
  and commentr = ']'
end

module SourceLex : Scanner.LEXICAL = Scanner.Lexical(SourceKey)

module SourceParsing : Parser_sig.PARSE = Parser.Parsing(SourceLex)


module ParseTerm : PARSE_TERM = struct
  open SourceParsing

  (*want this to be tok list -> type * tok list*)
let rec typp toks = ((key "Nat") >> (_ -> Source.Nat)
             |:| ((key "Unit") >> (_ -> Source.Unit))
             |:| ((circ (keycircl typp  "->") typp) >> Source.arr)
             |:| ((keycircl typp "Ref") >> Source.reftp)
             |:| ((keycircl typp "Comp") >> Source.comp)
             |:| (keycircr ")" (keycircl typp "("))
                    )
                    (*start here automate the surrounded by parens thing,
                    shows up in 3 places*)
let rec typed_id toks = keycircr ")" (circ typp (keycircr ":" (keycircl id "(")))

let rec term toks =                               
 (((circ term (*look for body of the lambda *)
      ((keycircr "."
          (keycircl
             (circ (repeat typed_id) typed_id) (*look for all the captured vars with
                                               type annotations*)
             "\\") (*looking for a lambda*)
       ) >> cons) (*collects identifiers into a list*)
   ) >> Source.absList) (*turns list of identifiers and body into a lam*)
  |:| ((circ (repeat atom) atom) >> Source.applyList) (*single atom or application of atoms*)
  |:| ((keycircl atom "ret") >> Source.ret) (*looking for a ret. make ret: exp -> exp*)
  |:| ((keycircl (keycircl (circ (keycircr ")" term) (*2nd term*)
                             (keycircr "," term)) (*1st term*)
               "(" )
         "bind") >> Source.bind) (*looking for a bind. make bind : exp x exp -> exp*)
  |:| ((keycircl atom "ret") >> Source.refe) (*looking for a ref exp *)
  |:| ((circ term (keycircr ":=" exp)) >> Source.asgn) (*: (string * exp) -> exp*)
  |:| ((keycircl id "!") >> Source.deref)) toks
and atom toks =  ((id >> Source.free)
                 |:| (keycircr ")" (keycircl term "("))
                 |:| (natp >> Source.nat)
                 |:| (starp >> Source.star)) toks (*: unit -> exp*)

let read s = match term (SourceLex.scan a) with
    (m, []) -> m
  | (_, _::_) -> raise SyntaxErr "Extra characters in phrase"
    end
