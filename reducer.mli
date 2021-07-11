open Core_kernel
(*beta reduction*)

module type REDUCER = sig 
  type store_type = (int, Source.exp, Int.comparator_witness) Map.t (*for storing references*)
  type env_type = (string, Source.typ * Source.exp, String.comparator_witness) Map.t (*for storing identifiers declared earlier in the file being evaluated*)
  exception RuntimeError of string
  val eval: env_type -> store_type -> Source.exp -> Source.exp * store_type

end

module Reducer : REDUCER
