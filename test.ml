include Base
include Core
include Core_kernel
include String_lib
include Scanner
include Parser
include Parser_sig
include Source
include Source_parsing
module MyString = String_lib

open Display

open Source_parsing

open Source

module Display_Source = Display(Source)

let testexp = Bind(Free("1starg"), Lam(("aa", Nattp), Bound(0)))

(*passing
  let test = "\\(x: Nat)(y: Unit).ref(x)"
let test1 = "\\(x: Nat)(y: Ref(Unit)).ref(x)"
*)

  let test = "\\(x: Nat)(y: Unit).ref(x)"
let test1 = "(\\(x: Nat)(y: Ref(Unit)).ref(x))"
let test2 = "\\(y: Ref(Unit)).y y"
let test3 = "f"
let test4 = "5"
let test5 = "()"
let test6 = "(\\(y: Ref(Unit)).y y)"
let test7 = test6 ^ " " ^ test1 ^ " " ^ test4

let v0 = Display_Source.printer (ParseTerm.read test)
let v1 = Display_Source.printer (ParseTerm.read test1)
let v2 = Display_Source.printer (ParseTerm.read test2)
let v2 = Display_Source.printer (ParseTerm.read test3)
let v2 = Display_Source.printer (ParseTerm.read test4)
let v2 = Display_Source.printer (ParseTerm.read test5)
let v2 = Display_Source.printer (ParseTerm.read test6)
let v2 = Display_Source.printer (ParseTerm.read test7)

