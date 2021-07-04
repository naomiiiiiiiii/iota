open Base
open Core_kernel
open Source

module type STATE = sig
  type store_type = (int, exp, Int.comparator_witness) Map.t (*for storing references*)
  type env_type = (string, exp, String.comparator_witness) Map.t (*for storing identifiers declared earlier in the file being evaluated*)
  val store : store_type 
  val env : env_type
end

module type REDUCER = sig 
  exception RuntimeError of string
  module State : STATE
  val eval: exp -> exp * State.store_type
end

(*probably need to put a with here*)
module Reducer (State: STATE): REDUCER = struct

  exception RuntimeError of string
  module State = State
  let env = State.env
  let store = State.store

  let get_loc loc = match loc with
      Loc n -> n
    | _ -> raise (RuntimeError ("expected location, got " ^ (Display.exp_to_string loc)))

(*adds a new reference containing m to s, returns the index of this new reference
and the new store *)
let add_ref m s = let index = (Map.length s) in match (Map.add s ~key:index ~data:m) with
   `Duplicate -> raise (RuntimeError ("tried to overwite location " ^ (Int.to_string index)))
  |`Ok snew -> (Loc index, snew)

let assign_ref loc m s = let index = (get_loc loc) in
  let change_fn = (fun v -> match v with
        None -> raise (RuntimeError ("assigned to uninitialized location " ^ (Int.to_string index)))
      | Some _ -> Some m
     ) in
  (Star, (Map.change s index ~f:change_fn))

let deref loc s = let index = (get_loc loc) in match (Map.find s index) with
    None -> raise (RuntimeError ("dereferencing uninitialized location " ^ (Int.to_string index)))
  | Some m -> (m, s)

let rec eval_help (m, s) = match m with
    Free id -> eval_help ((Map.find_exn env id), s)
  | Star | Nat _ | Loc _ | Lam _ | Ret _ | Ref _ | Asgn _ | Deref _ -> (m, s) (*ret, ref, asgn, deref are suspended computations*)
  | Ap(fn, arg) -> let (fnval, s1) = (eval_help (fn, s)) in
    let (argval, s2) = (eval_help (arg, s1)) in
    (match fnval with
      Lam(_, body) -> eval_help ((subst 0 argval body), s2)
     | _  -> (Ap(fnval, argval), s2))
  | Bind(m1, m2) -> let (m1cval, s1) = (eval_help (m1, s)) in (*e1cval should be ret, ref, asn, deref*)
    let (m1val, s2) = bind_eval(m1cval, s1) in (*releases the computation in e1cval*)
    eval_help (subst 0 m1val (snd m2), s2)
  | exception RuntimeError err -> raise (RuntimeError err)
  | Bound _ | exception _ -> raise (RuntimeError ("failing on " ^ (Display.exp_to_string m)))

(*evaluates what is underneath the structure that suspends m and carries out any effects
gives (m', s') where m' is the finished result of the once-delayed computation
and s' is the new store*)    
and bind_eval (m, s) = match m with
    Ret(m0) -> eval_help (m0, s)
  | Ref(m0) -> let (mval, s1) = eval_help(m0, s) in (add_ref mval s1)
  | Asgn(loc, m1) -> let (locval, s1) = eval_help(loc, s) in
    let (m1val, s2) = eval_help(m1, s1) in (assign_ref locval m1val s2) 
  | Deref(loc) -> let (locval, s1) = eval_help (loc, s) in (deref locval s1)
  | _ -> raise (RuntimeError ((Display.exp_to_string m) ^ " not bindable"))


let eval m = eval_help (m, store)

end
