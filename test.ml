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
include Reducer
include State

open Display
open Source

(*with type store_type =
                                 (int, exp, Int.comparator_witness) Map.t
                           and
                           type env_type = (string, exp, String.comparator_witness) Map.t  *)




let empty_env = (Map.empty (module String))
let empty_store = (Map.empty (module Int))

let testenv = Map.set empty_env ~key:"f" ~data:(Arr(Nattp, Reftp(Unit)),
                                                read("\\(x: Nat). ref(())"))

module Teststate : STATE = struct
  type store_type = (int, exp, Int.comparator_witness) Map.t (*for storing references*)
  type env_type = (string, typ * exp, String.comparator_witness) Map.t (*for storing identifiers declared earlier in the file being evaluated*)
  let store = empty_store 
  let env = testenv
end

module Reduce_Test = Reducer(Teststate)

open Reduce_Test
    (*passing
      let test = read "\\(x: Nat)(y: Unit).ref(x)"
let test = read "bind(ret(5), \\(x: Nat).ret ())"

let test = read "bind(ret(5), \\(x: Nat).ret ())"
let test1 = read "bind(ret(ref(5)), \\(x: Ref(Nat)).ret(!x))"
let test1 = read "f 5" with f in env
    *)

(*final tests for display*)

let test1 = "let shouldbe5 = bind(ret(5), \\n.bind(ref(0), \r.bind(r:=n, !r)))" (*r should come out 5*)

let test2 = "let theanswer = bind(ref(40), \\r1.bind(ref(!r1 + 1), \\r2. bind(r2 := !r2 + 1, !r2)))" (*r2 should come out 42*)

let test3= "let plus_ref = ref (\\(n1: Nat)\\(n2: Nat). n1 + n2
let plus1 = \\(n2: Nat).!plus_ref 1 n2
let shouldbetwo = plus1 1 
"

(*passing*)
let test0 = read "bind(ret(5), \\n.bind(ref(0), \\r.bind(r:=n, \\u.bind(!r, \\m.m))))"
let test = exp_to_string (fst (eval test0))
let v = print_endline(exp_to_string test0)
let v = print_endline(test)

