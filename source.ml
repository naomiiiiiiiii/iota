include Base
include Core
include Core_kernel
include String_lib
include Scanner
include Parser
include Source_sig
module MyString = String_lib

let o = Fn.compose



module Source : SOURCE = struct
  type typ = Nattp
           | Unit
           | Arr of typ * typ
           | Reftp of typ
           | Comp of typ

  type exp = Free of string
         | Bound of int
         | Star
         | Nat of int
         | Loc of int
         | Lam of (string * typ) * exp
         | Ap of exp * exp
         | Ret of exp
         | Bind of exp * exp
         | Ref of exp (*ref(e) evaluates to a location (key) after storing e at that location.
                      need to pass a particular ref(e) around as an argument wherever you go, two
                      ref(e) are not the same reference!*)
         | Asgn of exp * exp
         | Deref of exp



(*a lot of functions regarding exps involve iterating over
  the structure of a term, incrementing some nat argument when you go under a lambda
bc is what to do on the base case expressions
like mapi but for exp*)
let rec traverse (bc: int -> (string -> exp) * (int -> exp)) (start: int): exp -> exp
  = fun m ->
    let traverse_b = traverse bc in
    let bcs = bc start in
    match m with
    Free a -> (fst bcs) a
  | Bound i -> (snd bcs) i
  | Star -> m
  | Nat _ -> m
  | Loc _ -> m
  | Lam (p, m') -> Lam(p, (traverse_b (start + 1)) m')
  | Ap(m1, m2) -> Ap((traverse_b start m1), (traverse_b start m2))
  | Ret(m0) -> Ret(traverse_b start m0)
  | Bind(m1, m2) -> Bind((traverse_b start m1), (traverse_b (start + 1) m2))
  | Ref(v) -> Ref(traverse_b start v)
  | Asgn(r, e) -> Asgn((traverse_b start r), (traverse_b start e))
  | Deref r -> Deref(traverse_b start r)

(*folds accross an expression from the most nested part (rightmost part) upwards*)
let rec fold_expr (bc1: string -> 'a -> 'a) (bc2: int -> 'a -> 'a) (start: 'a): exp -> 'a
   = fun m ->
    let foldbc = fold_expr bc1 bc2 in
    match m with
    Free a -> bc1 a start
  | Bound i -> bc2 i start
  | Star -> start
  | Nat _ -> start
  | Loc _ -> start
  | Lam (_, m') -> foldbc start m'
  | Ap(m1, m2) -> foldbc (foldbc start m2) m1
  | Ret(m0) -> foldbc start m0
  | Bind(m1, m2) -> foldbc (foldbc start m2) m1
  | Ref(v) -> foldbc start v
  | Asgn(r, e) -> foldbc (foldbc start e) r
  | Deref r -> foldbc start r

let fvars = 
      let bc1 = fun a -> fun l -> a::l 
      and bc2 = fun _ -> fun l -> l 
    in
    fold_expr bc1 bc2 []


let abstract i x = let bc = fun i -> (
      let free = fun a -> if (String.equal a x) then (Bound i) else (Free a)
      and bound = fun j -> (Bound j)
      in (free, bound))
    in
    traverse bc i

let absList (l, m) = List.fold_right l ~f:(fun x -> fun m0 -> Lam(x, (abstract 0 (fst x) m0))) ~init:m

let applyList (m0, ms) = List.fold_left ms ~f:(fun bigapp -> fun mn -> Ap(bigapp, mn)) ~init:m0

(*shift i dot m shifts m's bound variables up by i, only ignoring variables <= dot*)
let shift i dot = let bc = fun dots -> (
      let free = fun a -> (Free a)
      and bound = fun j -> if j>=dots then Bound(j+i) else Bound j
      in (free, bound))
  in traverse bc dot

let subst i v = let bc = fun i' -> (
      let free = fun a -> Free a
      and bound = fun j ->if j<i' then Bound j
        else if j=i' then shift i 0 v (*v cannot capture any vars when you sub it in, so gotta shift it*)
        else Bound(j-1)
      in (free, bound))
  in traverse bc i


(*0 arg to traverse is useless; i just wanted to use my cool HOF instead of writing out
the cases*)
let rec inst env  = let bc = fun _ -> (
      let free = fun a -> try (inst env (Map.find_exn env a)) with (Not_found_s _) -> Free a
      and bound = fun j -> Bound j in
      (free, bound))
    in traverse bc 0

let free x = Free x
let ret x = Ret x
let bind (x, y) = Bind(x, y)
let refexp x = Ref(x)
let asgn(x, y) = Asgn(x, y)
let deref x = (print_endline "in deref"); Deref x
let star _ = Star
let nat x = Nat x
let loc x = Loc x
let arr (x, y) = Arr(x, y)
let reftp x = Reftp(x)
let comp x = Comp(x)


end
