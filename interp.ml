open Core_kernel

(*top level first thing that actually parses the string for real*)

(*can't be making new modules inside your main
can't make a new state module inside your main
options: make the state module a tuple
  make the state module a record?
make the state module mutable*)

(*how to scan declarations*)

(*starting up toplevel, begin with empty state*)


  let empty_store = Map.empty (module Int)
  let empty_env = Map.empty (module String)

let step_to_string name tau v = name ^ ":" ^ (Display.typ_to_string tau)
                                  ^ " =>* " ^ (Display.exp_to_string v)

module Reducer = Reducer.Reducer

module Checker = Checker.Checker

let print_out s = Out_channel.output_string stdout s; Out_channel.flush stdout

let rec main env store =
  print_out ">>";
let (name, in_exp) = Source_parser.read In_channel.(input_line_exn stdin) in (*get the first declaration*)
let in_type = Checker.checker env in_exp in
let (in_val, end_store) = Reducer.eval env store in_exp in (*want the checking for type errors to happen first so want this and line above to be sequential*) 
print_endline (step_to_string name in_type in_val);
main (Map.set env ~key:name ~data:(in_type, in_val)) end_store

let _ = main empty_env empty_store

(*file must have the form
let id = exp (repeated with newlines beteween)
interpreter typechecks and evaluates exp, updates env,
typechecks and evaluate next exp
could get it to even print out a little >> like a shell*)

(*val env : string x (exp * typ) map only values allowed in here, in particular
                                  no references (effectful exps), only locations (after store has been updated)*)

(*split the file into separate declarations by newlines*)
