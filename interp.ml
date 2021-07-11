open Core_kernel

(*the toplevel! the repl!*)

module Reducer = Reducer.Reducer
module Checker = Checker.Checker

let empty_store = Map.empty (module Int)
let empty_env = Map.empty (module String)

let step_to_string name tau v = name ^ ":" ^ (Display.typ_to_string tau)
                                  ^ " =>* " ^ (Display.exp_to_string v)
let print_out s = Out_channel.output_string stdout s; Out_channel.flush stdout

let rec main env store =
  print_out ">>";
let (name, in_exp) = Source_parser.read In_channel.(input_line_exn stdin) in 
let in_type = Checker.checker env in_exp in
let (in_val, end_store) = Reducer.eval env store in_exp in (*want the checking for type errors to happen first so want this and the line above to be sequential*) 
print_endline (step_to_string name in_type in_val);
main (Map.set env ~key:name ~data:(in_type, in_val)) end_store (*add successful declaration to environment*)

let _ = main empty_env empty_store
