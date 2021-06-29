include Base
include Core
include Core_kernel
include String_lib
module myString = String_lib
include Scanner
include Parser
include Parser_sig
include Source

let o = Fn.compose

let () = Printf.printf "hoog \n"

let rec rename a bs =
  if myString.exists a bs then rename (bs, a ˆ "’") else a


let strip (m, bs) = match m with
    Lambda.Abs(x,t) ->
    let val newx = rename (fvars t, x)
in strip (Lambda.subst 0 (Lambda.Free newx) t, newx :: bs)
 | _ -> (m, List.rev bs)
end

let stripAbs m = strip(m, [])

let spaceJoinr b acc = " " ˆ b ˆ acc

  let rec print_exp m = match m with
    Free a -> a
  | Bound i -> "umatched binder" ^ string_of_int(i) ^ "\n"
  | Star -> "()"
  | Nat n -> string_of_int(n)
  | Loc n -> "Address: " ^ string_of_int(n) ^ "\n"
  | Lam (y, m') -> let (b::bs, body) = stripAbs m in
    let front = "\\" ˆ b ˆ (List.fold_right spaceJoin ". " bs) in
    front ^ (print_exp m')
  | Ap(m1, m2) -> print_ap m
  | Ret(m0) -> "ret" ^ (atom m0)
  | Bind(m1, m2) -> "bind(" ^ (print_exp m1) ^ "," (print_exp m2) ^ ")"
  | Ref(v) -> "ref" ^ (atom v)
  | Asgn(r, e) -> (atom r) ^ ":=" ^ e
  | Deref r -> "!"^(atom r)
and print_ap m = match m with (*once print_app is entered all terms
                               that aren't simply identifiers will be
                                 wrapped in parens,
                              identifiers will have a space put in front*)
    Ap(m1, m2) -> (print_ap m1) ^ (atom m2)
  | _ -> atom m
and atom m = match m with
    Free(a) -> " "^a
  | _ -> "(" ^ (print_exp m) ^ ")"


