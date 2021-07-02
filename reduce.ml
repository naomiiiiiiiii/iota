open Core_kernel
open Source

module type STATE = sig
  val env : (string, exp, String.comparator_witness) Map.t
  val store : (int, exp, Int.comparator_witness) Map.t
end

module type REDUCER = sig 
exception RuntimeError of string

val eval: exp -> exp

end

module Reducer (State: STATE): REDUCER = struct

  exception RuntimeError of string

type stepopt = Step of exp | Val 

let try_step m = match m with
    Free id -> Step (Map.find_exn env id)
  | Star -> Val
  | Nat _ -> Val
  | Loc _ -> Val
  | Lam _ -> Val
  | Ap(fn, arg) -> match fn with

  | Bound _ | exception _ -> raise (RuntimeError ("failing on " ^ (exp_to_string m)))


