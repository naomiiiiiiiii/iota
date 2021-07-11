include Base
include Core
include Core_kernel

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
         | Plus of exp * exp
         | Lam of (string * typ) * exp
         | Ap of exp * exp
         | Ret of exp
         | Bind of exp * (string * exp)
         | Ref of exp
         | Asgn of exp * exp
         | Deref of exp

let rec typ_equal x y = match (x, y) with
    (Nattp, Nattp) -> true
  | (Unit, Unit) -> true
  | (Arr(x1, x2), Arr(y1, y2)) -> (typ_equal x1 y1) && (typ_equal x2 y2)
  | (Reftp(x1), Reftp(y1)) -> typ_equal x1 y1
  | (Comp(x1), Comp(y1)) -> typ_equal x1 y1
  | _ -> false

(*a lot of functions regarding exps involve iterating over
  the structure of a term, incrementing some nat argument when you go under a binder
bc is what to do on the base case expressions
think of it as like mapi but for exp*)
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
  | Plus (m1, m2) -> Plus (traverse_b start m1, traverse_b start m2)
  | Lam (p, m') -> Lam(p, (traverse_b (start + 1)) m')
  | Ap(m1, m2) -> Ap((traverse_b start m1), (traverse_b start m2))
  | Ret(m0) -> Ret(traverse_b start m0)
  | Bind(m1, (s, m2)) -> Bind((traverse_b start m1), (s, (traverse_b (start + 1) m2)))
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
  | Plus(m1, m2) -> foldbc (foldbc start m2) m1
  | Lam (_, m') -> foldbc start m'
  | Ap(m1, m2) -> foldbc (foldbc start m2) m1
  | Ret(m0) -> foldbc start m0
  | Bind(m1, (_ , m2)) -> foldbc (foldbc start m2) m1
  | Ref(v) -> foldbc start v
  | Asgn(r, e) -> foldbc (foldbc start e) r
  | Deref r -> foldbc start r

let fvars = 
      let bc1 = fun a -> fun l -> a::l 
      and bc2 = fun _ -> fun l -> l 
    in
    fold_expr bc1 bc2 []


(* abstract : int -> string -> exp -> exp
PRE: i >= 0
POST: abstract i x M will turn all free occurences of x into the bound variable i*)
let abstract i x = let bc = fun i -> (
      let free = fun a -> if (String.equal a x) then (Bound i) else (Free a)
      and bound = fun j -> (Bound j)
      in (free, bound))
    in
    traverse bc i


let absList (varlist, body) = List.fold_right varlist ~f:(fun var -> fun body0 -> Lam(var, (abstract 0 (fst var) body0))) ~init:body

let applyList (fn, args) = List.fold_left args ~f:(fun bigapp -> fun arg -> Ap(bigapp, arg)) ~init:fn

(* shift: int -> int -> exp -> exp
shift i dot m shifts m's bound variables up by i, only ignoring variables <= dot*)
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

let free x = Free x
let star _ = Star
let nat x = Nat x
let loc x = Loc x
let lam ((s, t), e) = Lam ((s, t), e)
let ap (m1, m2) = Ap (m1, m2)
let plus(x, y) = Plus(x, y)
let ret x = Ret x
let bind (m0, (name, m1)) = Bind(m0, (name, (abstract 0 name m1)))
let refexp x = Ref(x)
let asgn(x, y) = Asgn(x, y)
let deref x =  Deref x

let arr (x, y) = Arr(x, y)
let reftp x = Reftp(x)
let comp x = Comp(x)


