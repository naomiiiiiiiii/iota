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

  let get_loc loc = match loc with
      Loc n -> n
    | _ -> raise (RuntimeError ("expected location, got " ^ (Display.exp_to_string loc)))

(*adds a new reference containing m to s, returns the index of this new reference
and the new store *)
let add_ref m s = let index = (Map.length s) in match (Map.add s ~key:index ~data:m) with
    Ok snew -> (Loc index, snew)
  | Duplicate -> raise RuntimeError "tried to overwite location " ^ (Int.to_string index)

let assign_ref loc m s = let index = (get_loc loc)
  and change_fn = (fun v -> match v with
        None -> raise RuntimeError ("assigned to uninitialized location " ^ (Int.to_string index))
      | Some m_old -> Some m
     ) in
  (Star, (Map.change s index change_fn))

let deref loc s = let index = (get_loc loc) in match (Map.find s index) with
    None -> raise (RuntimeError ("dereferencing uninitialized location " ^ (Int.to_string index)))
  | Some m -> (m, s)

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
  | Bind(e1, e2) -> let (e1cval, s1) = (eval (e1, s)) in (*e1cval should be ret, ref, asn, deref*)
    let (e1val, s2) = bind_eval(e1cval, s1) in (*releases the computation in e1cval*)
   eval (subst 0 e1val e2, s2)
    
  | Bound _
  | exception RuntimeError _ -> m
  | exception _ -> raise (RuntimeError ("failing on " ^ (exp_to_string m)))

(*evaluates what is underneath the structure that suspends m and carries out any effects
gives (m', s') where m' is the finished result of the once-delayed computation
and s' is the new store*)    
and bind_eval(m, s) = match m with
    Ret(m0) -> eval (m0, s)
  | Ref(m0) -> let (mval, s1) = eval(m0, s) in (add_ref mval s1)
  | Assn(loc, m1) -> let (locval, s1) = eval(loc, s) in
    let (m1val, s2) = eval(m1, s1) in (assign_ref locval m1val s2) 
  | Deref(loc) -> let (locval, s1) = eval(loc, s) in (deref locval s1)
  | _ -> raise (RuntimeError ((exp_to_string m) ^ " not bindable"))


