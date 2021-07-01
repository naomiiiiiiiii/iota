include Base
include Core
include Core_kernel
include String_lib
include Scanner
include Parser
include Parser_sig
include Source
include Source_parser
module MyString = String_lib

open Display

open Source_parser

open Source

let testexp = Bind(Free("1starg"), Lam(("aa", Nattp), Bound(0)))

(*passing
  let test = "\\(x: Nat)(y: Unit).ref(x)"
let test1 = "\\(x: Nat)(y: Ref(Unit)).ref(x)"


let test = "\\(x: Nat)(y: Unit).ref(x)"
let test1 = "(\\(x: Nat)(y: Ref(Unit)).ref(x))"
let test2 = "\\(y: Ref(Unit)).y y"
let test3 = "f"
let test4 = "5"
let test5 = "()"
let test6 = "(\\(y: Ref(Unit)).y y)"
  let test7 = test6 ^ " " ^ test1 ^ " " ^ test4
let test6 = "(\\(y: Ref(Unit)).y y)"
let test8 = "ret x" 
  let test9 = "bind(\\(zzz: Nat).ref zzz, ret" ^ test6 ^ ")"

let test6 = "(\\(y: Ref(Unit)).y y)"
let test10 = "bind(\\zzz.ref zzz, ret" ^ test6 ^ ")"
let test10 = "\\(x: Nat -> Nat).!x"
let test12 = "\\(x: Ref(Nat -> Nat)).!x"
*)


let test11 = "\\(x: Comp((Ref Nat) -> Nat)).x:=5"


  let test = "\\(x: Nat)(y: Unit).ref(x)"
let v2 = printer (read test11)
(*let v3 = Display_Source.printer (ParseTerm.read test11)*)
