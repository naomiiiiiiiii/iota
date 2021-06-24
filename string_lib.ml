include Core
include Core_kernel

let getc = fun s -> try Some ((String.get s 1), (String.drop_prefix s 1))
  with Invalid_argument _ -> None

let mem l s = List.mem l s ~equal:String.equal

let isGraph c = Char.is_print(c) && (not (Char.is_whitespace(c)))

let isPunct c = isGraph(c) && (not (Char.is_alphanum c))
                                    
let splitl f s = let back = String.lstrip ~drop:f s in
     (String.prefix s (String.length back), back)


(*let sub_opt = fun s pos len -> try Some (sub s pos len) with
      Invalid_argument _ -> None

let tail_opt = fun s ->  Some (sub s 1 ((length s) - 1))
*)
     

                                                                  

