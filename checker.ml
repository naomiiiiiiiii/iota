include Base
include Core
include Core_kernel
include Source

module type TYPESTATE = sig
  val env : (string, typ, String.comparator_witness) Map.t
  (*val store : (int, typ, Int.comparator_witness) Map.t env is for identifiers at the top level
                                                       store is for expressions stored in locations,
                                                       user can't pick the identifier
  dont need store for type checking, only for evaluation i think*)
end

module type CHECKER = sig 
exception TypeError of string
(*for the bound varibales*)
type context_typ
val checker : exp -> typ
end

module Checker (State: TYPESTATE) : CHECKER = struct
exception TypeError of string

(*job of type checker is to check a term after it is fully instantiated
doing a list so that when i cons a (bound) variable on as (var 0) the indexes of the other
vars all get shifted up automatically*)
type context_typ = typ list 

(*restrict the interp env
for the types of free variables*)
let env = State.env (*only values allowed in here, in particular
                                   no references (effectful exps), only locations (after store has been updated)*)

(*keeps track of the types of locations
use ONLY for type chekcing a Loc(n)
  let store = State.store*)

(*extracts the type from the type option otau, or raises (TypeError error) if
otau = NONE*)
let get_type otau error = match otau with
    None -> raise (TypeError error)
  | Some tau -> tau 

let is_comp tau = match tau with
    Comp _ -> true
  | _ -> false

let rec checker_help g m = match m with
    Free s -> (get_type (Map.find env s) ("unbound identifier:" ^ s))
  | Bound i -> (get_type (List.nth g i) ("unbound variable:" ^ (Int.to_string i)))
  | Star -> Unit
  | Nat _ -> Nattp
  | Loc i -> raise (TypeError ("uninitialized location:" ^ (Int.to_string i)))
  | Lam ((_, tau0), m) -> Arr (tau0, (checker_help (tau0:: g) m))
  | Ap(fn, arg) -> let tau_arg = (checker_help g arg)
    and tau_fn = (checker_help g fn) in
    (match tau_fn with
      Arr(s, t) when (typ_equal s tau_arg) -> t
    | _ -> raise (TypeError ("cannot apply " ^ (Display.typ_to_string tau_fn) ^ " to " ^
                  (Display.typ_to_string tau_arg))))
  | Ret(m0) -> Comp (checker_help g m0)
  | Bind(m0, m1) -> let tau_arg = (checker_help g m0)
    and tau_fn = (checker_help g m1) in 
    (match tau_fn with
       Arr(tau_arg0, tau_out) when ((typ_equal (Comp tau_arg0) tau_arg) && (is_comp tau_out)) -> tau_out
     | _ -> raise (TypeError ("cannot bind " ^ (Display.typ_to_string tau_arg) ^
                                " into " ^ (Display.typ_to_string tau_fn)))
    )
  | Ref(m0) -> Reftp (checker_help g m0)
  | Asgn(loc, v) -> let tau_loc = (checker_help g loc)
    and tau_v = (checker_help g v) in
    (match tau_loc with
       Reftp(tau_loc0) when (typ_equal tau_loc0 tau_v) -> Unit
     | _ -> raise (TypeError ("cannot assign " ^ (Display.typ_to_string tau_loc) ^ "the value " ^
                   (Display.typ_to_string tau_v))
                  ))
  | Deref loc -> let tau_loc = (checker_help g loc) in
    (match tau_loc with
       Reftp(tau_loc0) -> tau_loc0
     | _ -> raise (TypeError ("cannot dereference " ^ (Display.typ_to_string tau_loc)))
    )

let checker = checker_help [] 

end
