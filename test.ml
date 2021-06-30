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

open Lang

open Source_parsing

let testexp = Bind(Free("1starg"), Lam(("aa", Nattp), Bound(0)))

let test1 = "\\x.ret(x)"

let v = printer testexp
let v1 = printer (ParseTerm.read test1)

