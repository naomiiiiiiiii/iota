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

module Lang = Source

open Lang

let rec rename bs a =
  if MyString.member bs a then rename  bs (a ^ "'") else a

(*make a way to display the types first, then combine newx and tau*)
let rec strip(bs, m) = match m with
    Lam(x, tau, t) ->
    let newx = rename (fvars t) x
    in strip ((newx, tau) :: bs, subst 0 (Free newx) t)
  | _ -> (List.rev bs, m)

let stripAbs m = strip([], m)

let spaceJoin b acc = " " ^ b ^ acc

  let rec print_exp m = match m with
    Free a -> a
  | Bound i -> "umatched binder" ^ string_of_int(i) ^ "\n"
  | Star -> "()"
  | Nat n -> string_of_int(n)
  | Loc n -> "Address: " ^ string_of_int(n) ^ "\n"
  | Lam _ -> let ((b, tau)::bs, body) = stripAbs m in
    let front = "\\" ^ b ^ (List.fold_right ~f:spaceJoin ~init:". " bs) in
    front ^ (print_exp body)
  | Ap _ -> print_ap m
  | Ret(m0) -> "ret" ^ (print_atom m0)
  | Bind(m1, m2) -> "bind(" ^ (print_exp m1) ^ "," ^ (print_exp m2) ^ ")"
  | Ref(v) -> "ref" ^ (print_atom v)
  | Asgn(r, e) -> (print_atom r) ^ ":=" ^ (print_exp e)
  | Deref r -> "!"^(print_atom r)
and print_ap m = match m with (*once print_app is entered all terms
                               that aren't simply identifiers will be
                                 wrapped in parens,
                              identifiers will have a space put in front*)
    Ap(m1, m2) -> (print_ap m1) ^ (print_atom m2)
  | _ -> print_atom m
and print_atom m = match m with
    Free(a) -> " "^a
  | _ -> "(" ^ (print_exp m) ^ ")"


let printer m = print_endline(print_exp m)
