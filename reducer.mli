open Core_kernel
open Source


(*module type STATE = sig
  type store_type = (int, exp, Int.comparator_witness) Map.t (*for storing references*)
  type env_type = (string, exp, String.comparator_witness) Map.t (*for storing identifiers declared earlier in the file being evaluated*)
  val store : store_type 
  val env : env_type
  end*)

module type REDUCER = sig 
  type store_type = (int, exp, Int.comparator_witness) Map.t (*for storing references*)
  type env_type = (string, typ * exp, String.comparator_witness) Map.t (*for storing identifiers declared earlier in the file being evaluated*)
  exception RuntimeError of string
  val eval: env_type -> store_type -> exp -> exp * store_type

end

module Reducer : REDUCER
