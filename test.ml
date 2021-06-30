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
let v = Display_Source.printer (ParseTerm.read test)
*)

(*failing*)
let test1 = "\\(x: Nat)(y: Ref(Unit)).ref(x)"

let v1 = Display_Source.printer (ParseTerm.read test1)

