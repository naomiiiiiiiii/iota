include Base
include Core
include Core_kernel
include Source

let rec typ_to_string = function
    Nattp -> "Nat"
  | Unit -> "Unit"
  | Arr(t1, t2) -> (typ_to_string t1) ^ "->" ^ (typ_to_string t2)
  | Reftp(t1) -> "Ref(" ^ (typ_to_string t1) ^ ")"
  | Comp(t1) -> "Comp(" ^ (typ_to_string t1) ^ ")"

(*rename : string list -> string -> string
POST: rename bs a returns an a' where a' \notin bs*)
let rec rename bs a =
  if String_lib.member bs a then rename bs (a ^ "'") else a

(* zero_to_free: string * Source.exp -> Source.exp
zero_to_free x m2 = m2[0 := Free(x)]*)
let zero_to_free (x, m2) = let newx = rename (Source.fvars m2) x in (newx, subst 0 (Free newx) m2)

(* stripAbs: Source.exp -> string list * Source.exp
stripAbs \(xn: tau_1)....\(x0: tau_n).M evaluates to (["(xn: tau1)", ... "(x0: taun)"], M') where M' = M[0 := Free(x0)...[n:= Free(xn)]*)
let stripAbs m = 
let rec stripAbs_help(names, m) = match m with
    Lam((x, tau), body) -> let (newx, newbody) = zero_to_free (x, body) in
    let x_annot =  "(" ^ newx ^ ":" ^ (typ_to_string tau) ^ ")" in
    stripAbs_help (x_annot :: names, newbody)
  | _ -> (List.rev names, m) in
stripAbs_help([], m)

let abs_prefix s1 s2 = "\\" ^ s1 ^ "." ^ s2

(*to keep track of whether i want a space in front of an identifier or not*)
let space = " "
let nospace = ""

let constant = function
  | Nat _ | Loc _ | Star -> true
  | _ -> false

  let rec exp_to_string m = match m with
    Free a -> a
  | Bound i -> "umatched binder" ^ string_of_int(i) ^ "\n"
  | Star -> "()"
  | Nat n -> string_of_int(n)
  | Loc n -> "Address: " ^ string_of_int(n) ^ "\n"
  | Plus(m1, m2) -> (atom_to_string m1 nospace) ^ "+" ^ (atom_to_string m2 nospace)
  | Lam _ -> let (names, body) = stripAbs m in
    abs_prefix (String.concat names) (exp_to_string body)
  | Ap _ -> ap_to_string m
  | Ret(m0) -> "ret" ^ (atom_to_string m0 space)
  | Bind(m1, (s, m2)) -> let (name, body) = zero_to_free (s, m2) in
    "bind(" ^ (exp_to_string m1) ^ ", " ^ (abs_prefix name (exp_to_string body)) ^ ")"
  | Ref(v) -> "ref" ^ (atom_to_string v space)
  | Asgn(r, e) -> (atom_to_string r nospace) ^ ":=" ^ (exp_to_string e)
  | Deref r -> "!"^(atom_to_string r nospace)
and ap_to_string m = match m with
    Ap(m1, m2) -> (ap_to_string m1) ^ (atom_to_string m2 space)
  | _ -> atom_to_string m nospace
and atom_to_string m sp = match m with (*sp argument determines whether there is a space before the atom or not*)
    Free(a) ->  sp ^ a  
  |  _ when (constant m) -> sp ^ (exp_to_string m)
  | _ -> sp ^ "(" ^ (exp_to_string m) ^ ")"


let print_typ m = print_endline(typ_to_string m)
let print_exp m = print_endline(exp_to_string m)

