open Core_kernel
open Source
(*job of type checker is to check a term after it is fully instantiated*)

(*start here consider using a first class module*)
module type TYPESTATE = sig
  type env_type = (string, typ, String.comparator_witness) Map.t (*for storing identifiers declared earlier in the file being evaluated*)
  val env : env_type
      (*everything that comes out of the store should be ref(_)*)
end

module type CHECKER = sig 
  exception TypeError of string
  type env_type
  val env : env_type
(*for the bound varibales
  type context_typ*)

(*restrict the interp env, given to us from the call to interp
  which will do the mutable state updates and then pass it back in as a paramater
  for the types of free variables*)

(*maybe bundle the env and the type_store together into one tuple?
record? module? called state
just implement everything for now and organize later*)
(*val env : (string, typ, String.comparator_witness) Map.t only values allowed in here, in particular
                                   no references (effectful exps), only locations (after store has been updated)*)

(*keeps track of the types of locations
  use ONLY for type chekcing a Loc(n)*)
    (*store is global to one call to interp
    given to us from interp as a paramater*)
(*val type_store: (int, typ, Int.comparator_witness) Map.t*)
(*maybe this should be an unbouded array
cuz the key (address) isn't given by the user
i have to produce it each time
probs best to use map with length and then always add it at the end
  since jane street doesnt have dynamic array*)

val checker : exp -> typ

end

module Checker (State: TYPESTATE) : CHECKER
