open Core_kernel

(*signature of a typechecker for iota (the source language) *)

module type CHECKER = sig
  type env_type = (string, Source.typ * Source.exp, String.comparator_witness) Map.t (*records the types of previously declared variables in the environment*) 
  exception TypeError of string
  val checker : env_type -> Source.exp -> Source.typ
end

module Checker : CHECKER
