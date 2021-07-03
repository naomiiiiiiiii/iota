open Core_kernel
open Source


module type STATE = sig
  type store_type = (int, exp, Int.comparator_witness) Map.t (*for storing references*)
  type env_type = (string, exp, String.comparator_witness) Map.t (*for storing identifiers declared earlier in the file being evaluated*)
  val store : store_typ 
  val env : env_typ
end

module type REDUCER = sig 
exception RuntimeError of string
type store_type
type env_type
val env: env_type
val eval: exp * store_type -> exp * store_type

end

module Reducer (State: STATE) : REDUCER
