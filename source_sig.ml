include Base
include Core
include Core_kernel
include String_lib
include Scanner
include Parser 
module MyString = String_lib
let o = Fn.compose

let () = Printf.printf "euioeuoieuroeureou \n"


module type SOURCE = sig

  type exp = Free of string
               | Bound of int
               | Star
               | Nat of int
               | Lam of string * exp
               | Ap of exp * exp
               | Ret of exp
               | Bind of exp * exp
               | Let_ref of string * exp * exp
               | Asgn of string * exp
               | Deref of string

(* start here: find a natural numbers type so you dont have to keep checking this
PRE: i >= 0
POST: abstract i x M will turn all free occurences of x into the bound variable i*)
  val abstract : int -> string -> exp -> exp

(*
absList [x1; ... ; xn] M = \x1, ... xn. M**)
  val absList : string list * exp -> exp

(*applyList (M, [M1; ... ; Mn]) = M M1 ... Mn*)
  val applyList : exp * exp list -> exp

(*
PRE: i >= 0
subst i v M = M[i := v], where i is a bound variable*)
  val subst : int -> exp -> exp -> exp

  (*for the references
    val store: (string, exp, String.comparator_witness) Map.t
one store per PROGRAM (typechecker and evaluator) not one store per language
  *)

  (*inst env M instantiates M with the variables defined in environment env*)
  val inst: (string, exp, String.comparator_witness) Map.t -> exp -> exp
  
end


