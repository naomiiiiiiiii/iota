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

module SourceParsing : (Parser_sig.PARSE with type token = SourceLex.token) = Parser.Parsing(SourceLex)


module ParseTerm : PARSE_TERM = struct
  open SourceParsing

  let constant =  try ((natp >> Source.nat)
                   |:| (starp >> Source.star)) with SyntaxErr _ -> raise (SyntaxErr "expected constant")

  (*want this to be tok list -> type * tok list*)
let rec typp (toks: SourceLex.token list) = ((key "Nat") >> (fun _ -> Source.Nattp)
             |:| ((key "Unit") >> (fun _ -> Source.Unit))
             |:| ((circ (keycircl typp  "->") typp) >> Source.arr)
             |:| ((keycircl typp "Ref") >> Source.reftp)
             |:| ((keycircl typp "Comp") >> Source.comp)
             |:| (keycircr ")" (keycircl typp "("))
                    ) toks
                    (*start here automate the surrounded by parens thing,
                    shows up in 3 places*)
let typed_id = keycircr ")" (circ typp (keycircr ":" (keycircl id "(")))

let singleton a = [a]

let rec term toks =
  print_endline("running term on " ^ (SourceLex.display_toks toks));
(((circ term (*look for body of the lambda *)
      ((keycircr "."
          (keycircl 
             (circ (repeat typed_id) typed_id) (*look for all the captured vars with
                                               type annotations**)
             "\\") (*looking for a lambda*)
       ) >> cons) (*collects identifiers into a list*)
   ) >> Source.absList) (*turns list of identifiers and body into a lam*)
 |:| ((circ (repeat atom) atom) >> Source.applyList) (*single atom or application of atoms*)
  |:| ((keycircl atom "ret") >> Source.ret) (*looking for a ret. make ret: exp -> exp*)
  |:| ((keycircl (keycircl (circ (keycircr ")" term) (*2nd term*)
                             (keycircr "," term)) (*1st term*)
               "(" )
         "bind") >> Source.bind) (*looking for a bind. make bind : exp x exp -> exp*)
  |:| ((keycircl atom "ref") >> Source.refexp) (*looking for a ref exp *)
  |:| ((circ term (keycircr ":=" term)) >> Source.asgn) (*: (exp * exp) -> exp*)
  |:| ((keycircl atom "!") >> Source.deref)
 |:| constant
    ) toks
and atom toks =  ((id >> Source.free)
                  |:| (keycircr ")" (keycircl term "("))
                 |:| constant) toks
                 
let read s = match term (SourceLex.scan s) with
    (m, []) -> m
  | (_, _::_) -> raise (SyntaxErr "Extra characters in phrase")
    end
