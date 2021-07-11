open Core_kernel

type typ = Nattp
           | Unit
           | Arr of typ * typ
           | Reftp of typ
           | Comp of typ

type exp = Free of string
         | Bound of int
         | Star
         | Nat of int
         | Loc of int
         | Plus of exp * exp
         | Lam of (string * typ) * exp
         | Ap of exp * exp
         | Ret of exp
         | Bind of exp * (string * exp)
         | Ref of exp
         | Asgn of exp * exp
         | Deref of exp

val typ_equal: typ -> typ -> bool

  (*iterators*)
val traverse: (int -> (string -> exp) * (int -> exp)) -> int -> exp -> exp
val fold_expr: (string -> 'a -> 'a) -> (int -> 'a -> 'a) -> 'a -> exp -> 'a

(*fvars M returns a list of the free vars in M*)
val fvars: exp -> string list

(* absList [x1; ... ; xn] M = \x1, ... xn. M*)
val absList : (string * typ) list * exp -> exp

(*applyList (M, [M1; ... ; Mn]) = M M1 ... Mn*)
val applyList : exp * exp list -> exp
 
(*
PRE: i >= 0
POST: subst i v M = M[var i := v]*)
val subst : int -> exp -> exp -> exp

(*i can't believe i can't use constructors as functions smh*)
val free: string -> exp
val star : unit -> exp
val nat : int -> exp
val loc : int -> exp
val plus : exp * exp -> exp
val lam : (string * typ) * exp -> exp
val ap : exp * exp -> exp
val ret: exp -> exp
val bind: exp * (string * exp) -> exp
val refexp:  exp -> exp
val asgn: exp * exp -> exp
val deref : exp -> exp

val arr : typ * typ -> typ
val reftp : typ -> typ
val comp : typ -> typ


