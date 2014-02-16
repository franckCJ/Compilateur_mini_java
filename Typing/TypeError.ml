type t =
  | Illegal_class_name of string
  | Used_class_name of string
  | Used_meth_name of string
  | Used_att_name of string

exception Error of t * Location.t;;

(* Les erreurs. *)
let report_error = function
  | Illegal_class_name s ->
      print_string "Illegal class name : ";
      print_endline s
  | Used_class_name s ->
      print_string "Second use of the same class name : ";
			print_endline s
  | Used_meth_name s ->
      print_string "Second use of the same method name : ";
			print_endline s
  | Used_att_name s ->
      print_string "Second use of the same attribute name : ";
			print_endline s

let illegal_class_name str loc =
  raise (Error(Illegal_class_name str, loc))

let used_class_name str loc =
  raise (Error(Used_class_name str, loc))

let used_meth_name str loc =
  raise (Error (Used_meth_name str, loc))

let used_att_name str loc =
  raise (Error (Used_att_name str, loc))
