(* Error type associated with executing exceptions *)
type t
exception Error of t * Location.t

(* print an error *)
val report_error : t -> unit

(* raise the various errors *)
val  unexpected_error : Location.t -> 'a
val undefined_attribute : string -> Location.t -> 'a
