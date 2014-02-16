(* Error type associated with typing exceptions *)
type t
exception Error of t * Location.t

(* print an error *)
val report_error : t -> unit

(* raise the various errors *)
val illegal_class_name : string -> Location.t -> 'a
val used_class_name : string -> Location.t -> 'a
val used_meth_name : string -> Location.t -> 'a
val used_att_name : string -> Location.t -> 'a
