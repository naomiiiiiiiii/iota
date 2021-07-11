open Base
open Core_kernel
open Source

module type REDUCER = sig 
  exception RuntimeError of string
  type store_type = (int, exp, Int.comparator_witness) Map.t 
  type env_type = (string, typ * exp, String.comparator_witness) Map.t
  val eval: env_type -> store_type -> exp -> exp * store_type
end

module Reducer : REDUCER = struct
  exception RuntimeError of string
  type store_type = (int, exp, Int.comparator_witness) Map.t 
  type env_type = (string, typ * exp, String.comparator_witness) Map.t

  let get_loc loc = match loc with
      Loc n -> n
    | _ -> raise (RuntimeError ("expected location, got " ^ (Display.exp_to_string loc)))

  let get_nat exp_n = match exp_n with
      Nat n -> n
    | _ -> raise (RuntimeError ("expected nat, got " ^ (Display.exp_to_string exp_n)))

(* add_ref: Source.exp -> store_type -> Sourc.exp * store_type
adds a new reference containing m to s, returns the index of this new reference and the new store *)
let add_ref m s = let index = (Map.length s) in match (Map.add s ~key:index ~data:m) with
   `Duplicate -> raise (RuntimeError ("tried to overwite location " ^ (Int.to_string index)))
  |`Ok snew -> (Loc index, snew)

(* add_ref: Source.exp -> Source.exp -> store_type -> Sourc.exp * store_type
updates location loc in s to contain m, returns * and the new store *)
let assign_ref loc m s = let index = (get_loc loc) in
  let change_fn = (fun v -> match v with
        None -> raise (RuntimeError ("assigned to uninitialized location " ^ (Int.to_string index)))
      | Some _ -> Some m
     ) in
  (Star, (Map.change s index ~f:change_fn))

(* deref: Source.exp -> store_type -> Sourc.exp * store_type
dereferences location loc in s, returns *loc and the new store *)
let deref loc s = let index = (get_loc loc) in match (Map.find s index) with
    None -> raise (RuntimeError ("dereferencing uninitialized location " ^ (Int.to_string index)))
  | Some m -> (m, s)

let eval env store_in m_in =
  let rec eval_help (m, s) =
  match m with
    Free id -> eval_help (snd (Map.find_exn env id), s)
    | Star | Nat _ | Loc _ | Lam _ | Ret _ | Ref _ | Asgn _ | Deref _ -> (m, s)
    (*ret, ref, asgn, deref are suspended computations*)
  | Plus(m1, m2) -> let (m1val, s1) = eval_help (m1, s) in
    let (m2val, s2) = eval_help (m2, s1) in
    (Nat ((get_nat m1val) + (get_nat m2val)), s2)
  | Ap(fn, arg) -> let (fnval, s1) = eval_help (fn, s) in
    let (argval, s2) = eval_help (arg, s1) in
    (match fnval with
      Lam(_, body) -> eval_help ((subst 0 argval body), s2)
     | _  -> (Ap(fnval, argval), s2))
  | Bind(m1, m2) -> let (m1cval, s1) = (eval_help (m1, s)) in
    let (m1val, s2) = bind_eval(m1cval, s1) in (*releases the computation in m1cval*)
    eval_help (subst 0 m1val (snd m2), s2)
  | exception RuntimeError err -> raise (RuntimeError err)
  | Bound _ | exception _ -> raise (RuntimeError ("failing on " ^ (Display.exp_to_string m)))
(* bind_eval : Source.exp * store type -> Source.exp * store_type
bind_eval (m, s) evaluates the suspended computation in m and carries out any effects
gives (m', s') where m' is the return value of the unsuspended computation and s' is the new store*)    
and bind_eval (m, s) = match m with
    Ret(m0) -> eval_help (m0, s)
  | Ref(m0) -> let (mval, s1) = eval_help(m0, s) in (add_ref mval s1)
  | Asgn(loc, m1) -> let (locval, s1) = eval_help(loc, s) in
    let (m1val, s2) = eval_help(m1, s1) in (assign_ref locval m1val s2) 
  | Deref(loc) -> let (locval, s1) = eval_help (loc, s) in (deref locval s1)
  | _ -> raise (RuntimeError ((Display.exp_to_string m) ^ " not bindable"))
in eval_help (m_in, store_in)

end
