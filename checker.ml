include Base
include Core
include Core_kernel
include Source

module type CHECKER = sig
  type env_type = (string, typ * exp, String.comparator_witness) Map.t
  exception TypeError of string
  val checker : env_type -> exp -> typ
end

module Checker : CHECKER = struct
  exception TypeError of string
  type env_type = (string, typ * exp, String.comparator_witness) Map.t 

(* using a list for the context instead of a fancier data structure because lists work nicely with de bruijn indices
when i cons a (bound, of course) variable onto the front of context (making it var 0)
the indexes of the other vars in the context all get shifted up automatically*)
type context_typ = typ list 

(*like value_exn only it raises a TypeError*)
let value_Texn opt error = match opt with
    None -> raise (TypeError error)
  | Some tau -> tau 

let get_comptype ctau = match ctau with
    Comp(tau) -> tau
  | _ -> raise (TypeError ("expected comp type, got " ^ (Display.typ_to_string ctau))) 

let is_comp tau = match tau with
    Comp _ -> true
  | _ -> false

let checker env = 
let rec checker_help (g : context_typ) m = match m with
    Free id -> fst (value_Texn (Map.find env id) ("unbound identifier:" ^ id))
  | Bound i -> (value_Texn (List.nth g i) ("unbound variable:" ^ (Int.to_string i)))
  | Star -> Unit
  | Nat _ -> Nattp
  | Loc i -> raise (TypeError ("uninitialized location:" ^ (Int.to_string i))) (*raw locations should only show up during execution, not when a program is typechecked*)
  | Plus(m1, m2) -> let tau1 = checker_help g m1 and tau2 = checker_help g m2 in
    (match (tau1, tau2) with
      (Nattp, Nattp) -> Nattp
    | _ -> raise (TypeError
                    ("cannot add " ^ (Display.typ_to_string tau1) ^ " to " ^ (Display.typ_to_string tau2)))
    )
  | Lam ((_, tau0), m) -> Arr (tau0, (checker_help (tau0:: g) m))
  | Ap(fn, arg) -> let tau_fn = (checker_help g fn) and tau_arg = (checker_help g arg) in 
    (match tau_fn with
      Arr(s, t) when (typ_equal s tau_arg) -> t
    | _ -> raise (TypeError ("cannot apply " ^ (Display.typ_to_string tau_fn) ^ " to " ^
                             (Display.typ_to_string tau_arg)))
    )
  | Ret(m0) -> Comp (checker_help g m0)
  | Bind(m0, ( _, m1)) -> let tau_arg = get_comptype ((checker_help g m0))
    in let tau_out = (checker_help (tau_arg::g) m1) in
    if (is_comp tau_out) then tau_out else raise (TypeError ("cannot bind " ^ (Display.typ_to_string (Comp tau_arg)) ^
                                                             " into " ^ (Display.typ_to_string tau_out))
                                                 )
  | Ref(m0) -> Comp (Reftp (checker_help g m0)) (*a reference is a delayed computation which evaluates to a location after the store has been modified*)
  | Asgn(loc, v) -> let tau_loc = (checker_help g loc)
    and tau_v = (checker_help g v) in
    (match tau_loc with
       Reftp(tau_contents) when (typ_equal tau_contents tau_v) -> Comp(Unit)
     | _ -> raise (TypeError ("cannot assign a " ^ (Display.typ_to_string tau_loc) ^ " a value of type " ^
                   (Display.typ_to_string tau_v))
                  )
    )
  | Deref loc -> let tau_loc = (checker_help g loc) in
    (match tau_loc with
       Reftp(tau_contents) -> Comp(tau_contents)
     | _ -> raise (TypeError ("cannot dereference " ^ (Display.typ_to_string tau_loc)))
    )
in checker_help []
 

end
