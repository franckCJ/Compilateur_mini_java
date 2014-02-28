type t =
  | Unexpected_Error
  | Undefined_attribute of string

exception Error of t * Location.t;;

(* Execution Errors *)
let report_error = function
  | Unexpected_Error ->
      print_endline "Unexpected Error"
  | Undefined_attribute s ->
      print_string "The attribute ";
      print_string s;
      print_string " is not yet instanciated"

let unexpected_error loc =
	raise (Error (Unexpected_Error,loc))

let undefined_attribute var var_type exp_type loc =
	raise (Error (Undefined_attribute s,loc))
