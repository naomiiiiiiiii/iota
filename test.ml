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
                                                read("\(x: Nat). ref(())"))

module Teststate : STATE = struct
  type store_type = (int, exp, Int.comparator_witness) Map.t (*for storing references*)
  type env_type = (string, typ * exp, String.comparator_witness) Map.t (*for storing identifiers declared earlier in the file being evaluated*)
  let store = empty_store 
  let env = testenv
end

module Reduce_Test = Reducer(Teststate)

module Checker_Test = Checker(Teststate)

open Reduce_Test
open Checker_Test

    (*passing
      let test = read "\(x: Nat)(y: Unit).ref(x)"
let test = read "bind(ret(5), \(x: Nat).ret ())"

let test = read "bind(ret(5), \(x: Nat).ret ())"
let test1 = read "bind(ret(ref(5)), \(x: Ref(Nat)).ret(!x))"
let test1 = read "f 5" with f in env
    *)

(*final tests for display*)

(*shows ret bound*)
let test1 = "let shouldbe5 = bind(ret(5), \n.bind(ref(0), \r.bind(r:=n, \u.bind(!r, \m.ret m))))"

(*shows example error*)
let test2f = "bind(ref(40), \r1.bind(ref((!r1) + 1),
\r2. bind(r2 := (!r2) + 1, \u.bind(!r2, \n.n))))"
let test2 =
  "let typerror = " ^ test2f (*r2 should come out 42*)

 (*shows multiple refs, r2 = !r2 + 1*)
"let theanswer = bind(ref(40), \r1.bind(!r1, \n1.bind(ref(n1 + 1), \r2.bind(!r2, \n2.bind(r2 := n2 + 1, \u.bind(!r2, \n3. ret n3))))))"

(*shows environemtn and ref of weird type, dereffing a nonid*)
let test3= "
let plus = \(n1: Nat)(n2: Nat). n1 + n2
let four = plus (plus 1 1) 2
let plusref =ref plus
let getplusref = \(u : Unit).plusref
let plus1 = \(n2: Nat).bind(getplusref (), \fref. bind(!fref, \f. ret(f 1 n2)))
let twobutsuspended = plus1 1
let two = bind(twobutsuspended, \n.ret(n))
"

(*passing*)
  (*test0 is
  let r1 = ref(40)
  let r2 = ref(!r + 1)
  r2 := !r2 + 1
  let return = !r2*)
let test0 = read "bind(ref(40), \r1.bind(!r1, \n1.bind(ref(n1 + 1),
\r2.bind(!r2, \n2.bind(r2 := n2 + 1, \u.bind(!r2, \n3. n3))))))"
let v = print_endline(exp_to_string test0)
let test = exp_to_string (fst (eval test0))
let v = print_endline(test)

let testfail = read test2f
let v = print_endline (typ_to_string (checker testfail))

