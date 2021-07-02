open Core_kernel
open Source

module type STATE = sig
  type store_type = (int, exp, Int.comparator_witness) Map.t
  type env_type = (string, exp, String.comparator_witness) Map.t
  val store : store_typ 
  val env : env_typ
end

module type REDUCER = sig 
exception RuntimeError of string
type store_type
val eval: exp * store_type -> exp * store_type

end

(*probably need to put a with here*)
module Reducer (State: STATE): REDUCER = struct

  exception RuntimeError of string

  type store_typ = State.store_type

type stepopt = Step of (exp, store_typ) | Val 

let rec eval (m, s) = match m with
    Free id -> eval ((Map.find_exn env id), s)
  | Star -> (m, s)
  | Nat _ -> (m, s)
  | Loc _ -> (m, s)
  | Lam _ -> (m, s)
  | Ap(fn, arg) -> let (fnval, s1) = (eval (fn, s)) in
    let (argval, s2) = (eval (arg, s1)) in
    (match fnval with
      Lam(_, body) -> eval ((subst 0 argval body), s2)
     | _  -> (Ap(fnval, argval), s2))
  | Ret(c) -> (m, s) (*suspended! *)
  | Bind(e1, e2) -> let (e1cval, s1) = (eval (e1, s)) in (*unsuspends it!*)
    let e1val = get_comp e1cval in (eval (subst 0 e1val e2), s1)
  |
  | Bound _ | exception _ -> raise (RuntimeError ("failing on " ^ (exp_to_string m)))


