(*include module type of Source_sig

module Lang: Source_sig.SOURCE

(*check a term after full instantiated*)
val env : string x Lang.exp map (*only values allowed in here, in part
                                  no references (effectful exps), only locations (after store has been updated)*)
val context : int x Lang.exp map

val type_store: int x Lang.type 
*)
