include Base
include Core
include Core_kernel
include String_lib
include Scanner
include Parser
include Parser_sig
include Source
module MyString = String_lib

open Display

open Lang

let testexp = Bind(Free("1starg"), Lam("aa", Bound(0)))

let v = printer(testexp)

