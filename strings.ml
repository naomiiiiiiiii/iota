module type STRING_LIB = sig
  val get_opt : string -> int -> char option
  val sub_opt : string -> int -> int -> string option
end

module String_lib = struct
  include String
  let get_opt = fun s i -> try Some (get s i) with Invalid_argument _ -> None
  let sub_opt = fun s pos len -> try Some (sub s pos len) with
      Invalid_argument _ -> None
end


                                                                  

