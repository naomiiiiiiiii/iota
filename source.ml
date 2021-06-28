include Base
include Core
include Core_kernel
include String_lib
include Scanner
include Parser
include 
module MyString = String_lib

let o = Fn.compose

let () = Printf.printf "euioeuoieuroeureou \n"

module Source : SOURCE = struct
type exp = Free of string
               | Bound of int
               | Star
               | Nat of int
               | Lam of string * exp
               | Ap of exp * exp
               | Ret of exp
               | Bind of exp * exp
               | Let_ref of string * exp * exp
               | Asgn of string * exp
               | Deref of string

(*a lot of functions regarding exps involve iterating over
  the structure of a term, incrementing some nat argument when you go under a lambda
bc is what to do on the base case expressions*)
let rec traverse (bc: int -> (string -> exp) * (int -> exp) * exp * (int -> exp)) (start: int)
  = fun m ->
    let traverse_b = traverse bc in
    let bcs = bc start in
    match m with
    Free a -> fst bcs a
  | Bound i -> fst2 bcs i
  | Star -> fst3 bcs
  | Nat n -> fst4 bcs n
  | Lam (y, m') -> Lam(y, (traverse_b (start + 1)) m)
  | Ap(m1, m2) -> Ap((traverse_b start m1), (traverse_b start m2))
  | Ret(m0) -> Ret(traverse_b start m0)
  | Bind(m1, m2) -> Bind((traverse_b start m1), (traverse_b (i + 1) x m2))
  | Let_ref (r, v, e) -> Let_ref(r, (traverse_b start v), (traverse_b start e))
  | Asgn(r, e) -> Asgn(r, (traverse_b start e))
  | Deref(r) -> m


let rec abstract i x m = let bc = fun i -> (
      let free = fun a -> if (String.equal a x) then (Bound i) else (Free a)
      and let other = fun x -> x in
        (free, other, other, other))
    in
    traverse bc i

let absList (l, m) = List.fold_right l ~f:(fun x -> fun m0 -> Lam(x, abstract 0 x m0)) ~init:m

let applyList (m0, ms) = List.fold_left ms ~f:(fun mn -> fun bigapp -> Ap(bigapp, mn)) ~init:m0

(*shift i dot m shifts m's bound variables up by i, only ignoring variables <= dot*)
let shift i dot m = function
    (Free a) -> m 
  | Bound _ -> if j>=d then Bound(j+i) else Bound j
  | Star -> m
  | Nat _ -> m
  | Lam (y, m') -> Lam(y, (shift i x m'))
  | Ap(m1, m2) -> Ap((abstract i x m1), (abstract i x m2))
  | Ret(m0) -> Ret(abstract i x m0)
  | Bind(m1, m2) -> Bind((abstract i x m1), (abstract (i + 1) x m2))
  | Let_ref (r, v, e) -> Let_ref(r, (abstract i x v), (abstract i x e))
  | Asgn(r, e) -> Asgn(r, (abstract i x e))
  | Deref(r) -> m

end
