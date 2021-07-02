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
let testenv = Map.set empty ~key:"f" ~data:(Arr(Nattp, Reftp(Unit)))

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


let test1 = read "f 5"
let v = print_endline(typ_to_string (checker test1))

