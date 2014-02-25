(* Error type associated with typing exceptions *)
type t
exception Error of t * Location.t

(* print an error *)
val report_error : t -> unit

(* raise the various errors *)
val illegal_class_name : Type.t -> Location.t -> 'a
val used_class_name : Type.t -> Location.t -> 'a
val used_meth_name : Type.t -> Location.t -> 'a
val used_att_name : Type.t -> Location.t -> 'a
val inheritance_loop : Type.t -> Location.t -> 'a
val non_existing_class : Type.t -> Location.t -> 'a
val non_existing_method : Type.t -> string -> Location.t -> 'a
val non_existing_attribute : Type.t -> string -> Location.t -> 'a
val args_error : string -> int -> Location.t -> 'a
val return_type_error : string -> Type.t -> Type.t -> Location.t -> 'a
val sig_error : string -> Location.t -> Location.t -> 'a
val incorrect_type : Type.t -> Location.t -> Type.t -> Location.t -> Location.t -> 'a
val incorrect_var_type : string -> Type.t -> Type.t -> Location.t -> 'a
val non_typed_exp : Location.t -> 'a
