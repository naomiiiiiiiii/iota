include Base
include Core
include Core_kernel
include String_lib
include Scanner
include Parser
include Parser_sig
include Source
module MyString = String_lib

let o = Fn.compose

module Display (Lang: Source_sig.SOURCE) : (Display_sig.DISPLAY with type exp = Lang.exp
                                                                 and type typ = Lang.typ) = struct
  type exp= Lang.exp
  type typ = Lang.typ

open Lang

let rec type_to_string = function
    Nattp -> "Nat"
  | Unit -> "Unit"
  | Arr(t1, t2) -> (type_to_string t1) ^ "->" ^ (type_to_string t2)
  | Reftp(t1) -> "Ref(" ^ (type_to_string t1) ^ ")"
  | Comp(t1) -> "Comp(" ^ (type_to_string t1) ^ ")"


let rec rename bs a =
  if MyString.member bs a then rename  bs (a ^ "'") else a

let rec strip(bs, m) = match m with
    Lam((x, tau), t) ->
    let newx = rename (fvars t) x in
        let x_annot =  "(" ^ newx ^ ":" ^ (type_to_string tau) ^ ")"
    in strip (x_annot :: bs, subst 0 (Free newx) t)
  | _ -> (List.rev bs, m)

let stripAbs m = strip([], m)

let spaceJoin b acc = " " ^ b ^ acc

  let rec exp_to_string m = match m with
    Free a -> a
  | Bound i -> "umatched binder" ^ string_of_int(i) ^ "\n"
  | Star -> "()"
  | Nat n -> string_of_int(n)
  | Loc n -> "Address: " ^ string_of_int(n) ^ "\n"
  | Lam _ -> let (b::bs, body) = stripAbs m in
    let front = "\\" ^ b ^ (List.fold_right ~f:spaceJoin ~init:". " bs) in
    front ^ (exp_to_string body)
  | Ap _ -> ap_to_string m
  | Ret(m0) -> "ret" ^ (atom_to_string m0)
  | Bind(m1, m2) -> "bind(" ^ (exp_to_string m1) ^ "," ^ (exp_to_string m2) ^ ")"
  | Ref(v) -> "ref" ^ (atom_to_string v)
  | Asgn(r, e) -> (atom_to_string r) ^ ":=" ^ (exp_to_string e)
  | Deref r -> "!"^(atom_to_string r)
and ap_to_string m = match m with (*once ap_to_stringp is entered all terms
                               that aren't simply identifiers will be
                                 wrapped in parens,
                              identifiers will have a space put in front*)
    Ap(m1, m2) -> (ap_to_string m1) ^ (atom_to_string m2)
  | _ -> atom_to_string m
and atom_to_string m = match m with
    Free(a) -> " "^a
  | _ -> "(" ^ (exp_to_string m) ^ ")"


let printer m = print_endline(exp_to_string m)

end
