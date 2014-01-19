type t

type func

(* conversions between types and string *)
val stringOf : t -> string
val fromString : string -> t

val string_of_func : func -> string
val create_func : t list * t -> func
