(*top level first thing that actually parses the string for real*)

(*file must have the form
let id = exp (repeated with newlines beteween)
interpreter typechecks and evaluates exp, updates env,
typechecks and evaluate next exp
could get it to even print out a little >> like a shell*)

val env : string x exp * typ map (*only values allowed in here, in particular
                                  no references (effectful exps), only locations (after store has been updated)*)

(*split the file into separate declarations by newlines*)
