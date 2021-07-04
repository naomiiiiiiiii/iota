open Source


(*module type STATE = sig
  type store_type = (int, exp, Int.comparator_witness) Map.t (*for storing references*)
  type env_type = (string, exp, String.comparator_witness) Map.t (*for storing identifiers declared earlier in the file being evaluated*)
  val store : store_type 
  val env : env_type
  end*)

module type REDUCER = sig 
  exception RuntimeError of string
  module State : State.STATE
  val eval: exp -> exp * State.store_type

end

module Reducer (State: State.STATE) : REDUCER
