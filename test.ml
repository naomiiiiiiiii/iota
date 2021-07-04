include Base
include Core
include Core_kernel
include String_lib
include Scanner
include Parser
include Parser_sig
include Source
include Source_parser
include Checker
module MyString = String_lib

open Display
open Source

let empty = (Map.empty (module String))

module State: TYPESTATE = struct
  let env = testenv 
end

module Checker_Test = Checker(State)

open Checker_Test
    (*passing
      let test = read "\\(x: Nat)(y: Unit).ref(x)"
let test = read "bind(ret(5), \\(x: Nat).ret ())"

let test = read "bind(ret(5), \\(x: Nat).ret ())"
let test1 = read "bind(ret(ref(5)), \\(x: Ref(Nat)).ret(!x))"
let test1 = read "f 5" with f in env
    *)

(*final tests for display*)

let test1 = "let shouldbe5 = bind(ret(5), \n.bind(ref(0), \r.bind(r:=n, !r)))" (*r should come out 5*)

let test2 = "let theanswer = bind(ref(40), \r1.bind(ref(!r1 + 1), \r2. bind(r2 := !r2 + 1, !r2)))" (*r2 should come out 42*)

let test3= "let plus_ref = ref (\(n1: Nat)\(n2: Nat). n1 + n2
let plus1 = \(n2: Nat).!plus_ref 1 n2
let shouldbetwo = plus1 1 
"


let test1 = read "f 5"
let v = print_endline(typ_to_string (checker test1))

