include Base
include Core
include Core_kernel
include Source

module type TYPESTATE = sig
  val Env : (string, typ, String.comparator_witness) Map.t
  val Store : (int, typ, Int.comparator_witness) Map.t (*env is for identifiers at the top level
                                                       store is for expressions stored in locations,
                                                       user can't pick the identifier*)
end

module type CHECKER = sig 
exception TypeError of string
(*for the bound varibales*)
type Context
val checker : exp -> typ
end

module Checker (State: TYPESTATE) : CHECKER = struct
exception TypeError of string

(*job of type checker is to check a term after it is fully instantiated
doing a list so that when i cons a (bound) variable on as (var 0) the indexes of the other
vars all get shifted up automatically*)
type Context = typ list 

(*restrict the interp env
for the types of free variables*)
val env = State.Env (*only values allowed in here, in particular
                                   no references (effectful exps), only locations (after store has been updated)*)

(*keeps track of the types of locations
use ONLY for type chekcing a Loc(n)*)
val store = State.Store

(*extracts the type from the type option otau, or raises (TypeError error) if
otau = NONE*)
val get_type otau error = match otau with
    None -> raise (TypeError error)
  | Some tau -> tau 


val checker_help g m = match m with
    Free s -> (get_type (Map.find env s) ("unbound identifier:" ^ s))
  | Bound i -> (get_type (List.nth G i) ("unbound variable:" ^ (Int.to_string i)))
  | Star -> Unit
  | Nat _ -> Nattp
  | Loc i -> raise TypeError ("uninitialized location:" ^ (Int.to_string i))
  | Lam (_, tau0, m) -> Arr (tau0, (checker_help (tau0:: G) m))
  | Ap(fn, arg) -> let tau_arg = (checker_help g arg)
    and tau_fn = (checker_help g fn) in
    (match tau_fn with
      Arr(s, t) when (s == tau_arg) -> t
    | _ -> raise (TypeError ("cannot apply " ^ (Display.print_typ tau_fn) ^ "to" ^
                  (Display.print_typ tau_arg))))
  | Ret(m0) -> Comp (checker_help g m0)
  | Bind(m0, m1) -> let tau_arg = (checker_help g m0) in
    (match tau_arg with
       Comp(tau_arg0) -> (checker_help (tau_arg0::g) m1)
     | _ -> raise (TypeError ("cannot bind "^(Display.print_typ tau_arg)))
    )
  | Ref(m0) -> Reftp (checker_help g m0)
  | Asgn(loc, v) -> let tau_loc = (checker_help g loc)
    and tau_v = (checker_help g v) in
    (match tau_loc with
       Reftp(tau_loc0) when (tau_loc0 == tau_v) -> Unit
     | _ -> raise (TypeError ("cannot assign " ^ (Display.print_typ tau_loc) ^ "the value " ^
                   (Display.print_typ tau_v))
                  ))
  | Deref loc -> let tau_loc = (checker_help g loc) in
    (match tau_loc with
       Reftp(tau_loc0) -> tau_loc0
     | _ -> raise (TypeError ("cannot dereference " ^ (Display.print_typ tau_loc)))
    )

  val checker M = checker_help (Map.empty Int.comparator)

