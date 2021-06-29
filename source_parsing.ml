include Base
include Core
include Core_kernel
include String_lib
include Scanner
include Parser
include Source
module MyString = String_lib
let o = Fn.compose

(*shouldnt ever need to parse types,just print them from AST, going other way*)

let () = Printf.printf "hurg \n"

module type PARSE_TERM = sig
  val read : string -> Source.exp end

module SourceKey : Scanner.KEYWORD = struct
  let alpha_num = ["ret"; "bind"; "let"; "ref"; "in"]
  and symbols = ["("; ")"; "\\"; "="; ":="; "!"]
  and commentl = '['
  and commentr = ']'
end

module SourceLex : Scanner.LEXICAL = Scanner.Lexical(SourceKey)

module SourceParsing : Parser.PARSE = Parser.Parsing(SourceLex)


module ParseTerm = struct
  open Parser
    let test = Parser.epsilon end
 (*let rec parse_term toks =                                 
  (* *)((circ term (*look for body of the lambda *)
      ((keycircr "."
          (keycircl
             (circ (repeat id) id) (*look for all the captured vars*)
             "\\") (*looking for a lambda*)
       ) >> cons) (*collects identifiers into a list*)
   ) >> Source.absList) (*turns list of identifiers and body into a lam*)
       |:| ((circ (repeat atom) atom) >> Source.applyList) **) (*single atom or application of atoms*)
 (* |:| ((keycircl atom "ret") >> Source.Ret) (*looking for a ret. make ret: exp -> exp*)
  |:| ((keycircl (keycircl (circ (keycircr ")" term) (*2nd term*)
                             (keycircr "," term)) (*1st term*)
               "(" )
         "bind") >> Source.Bind) (*looking for a bind. make bind : exp x exp -> exp*)
  |:| ((circ (keycircl term "in")
    (circ (keycircl (keycircl term "ref") "=")
  
    (keycircl id "let"))) >> Source.Let_ref) (*looking for a ref declaration
                                         makeref: ((string x exp) x exp) -> exp *)
  |:| ((circ term (keycircr ":=" id)) >> Source.Assign) (*: (string * exp) -> exp*)
  |:| ((keycircl id "!") >> Source.Deref) (*: string -> exp*)
  (|:|) (startp >> Source.Star) (*: unit -> exp*)
and parse_atom toks =  (id >> free)
                               |:| ((keycircr ")" (keycircl term "(")) >> pi1)
                               |:| (natp >> nat)*)
