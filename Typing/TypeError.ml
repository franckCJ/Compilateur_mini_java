type t =
  | Illegal_class_name of Type.t
  | Illegal_inheritance of string * Type.t
  | Used_class_name of Type.t
  | Used_meth_name of string
  | Used_att_name of string
  | Inheritance_loop of Type.t
  | Non_existing_class of Type.t
  | Non_existing_method of Type.t * string
  | Non_existing_attribute of Type.t * string
  | Args_Error of string * int
  | Return_Error of string * Type.t * Type.t
  | Sig_Error of string * Location.t
  | Incorrect_type of Type.t * Type.t
  | Incorrect_var_type of string * Type.t * Type.t
  | Non_typed_exp

exception Error of t * Location.t;;

(* Typing Errors *)
let report_error = function
  | Illegal_class_name t ->
      print_string "Illegal class name : ";
      print_endline (Type.stringOf t) 
  | Illegal_inheritance (s,t) ->
      print_string "Class ";
      print_string s;
      print_string " cannot inherit from protected type ";
      print_endline (Type.stringOf t) 
  | Used_class_name t ->
      print_string "A class named ";
			print_string (Type.stringOf t);
			print_endline " has already been defined"
  | Used_meth_name s ->
      print_string "A method named ";
			print_string s;
			print_endline " has already been defined"
  | Used_att_name s ->
      print_string "An attribute named ";
			print_string s;
			print_endline " has already been defined"
  | Inheritance_loop t ->
      print_string "Inheritance loop for the class : ";
			print_endline (Type.stringOf t)
	| Non_existing_class t ->
      print_string "The class ";
			print_string (Type.stringOf t);
			print_endline " doesn't exist"
	| Non_existing_method (t,m) ->
      print_string "The method ";
			print_string m;
			print_string " is not defined in the class ";
			print_endline (Type.stringOf t)
	| Non_existing_attribute (c,a) ->
      print_string "The attribute ";
			print_string a;
			print_string " is not defined in the class ";
			print_endline (Type.stringOf c)
	| Args_Error (m,id) ->
		begin
		match id with 
			| 1		-> print_endline ("Too many arguments are given to the method : " ^ m)
			| -1 	-> print_endline ("Too few arguments are given to the method : " ^ m)
			| _ 	-> print_endline ("Wrong type of argument for the method : " ^ m)
		end
	| Return_Error (m,return_type,exp_type) ->
		print_string "The body of ";
		print_string m;
		print_string (" has a type " ^ (Type.stringOf exp_type));
		print_string " which doesn't validate the return type : ";
		print_endline (Type.stringOf return_type)
	| Sig_Error (m,loc) ->
		print_string "The method ";
		print_string m;
		print_string " has not the same signature as define in a parent class here : ";
		Location.print loc
	| Incorrect_type (type1,type2) ->
		print_string ("The expression has type " ^ (Type.stringOf type1));
		print_string (" but an expression of type " ^ (Type.stringOf type2));
		print_endline " was expected"
	| Incorrect_var_type (var,var_type,exp_type) ->
		print_string "The expression has a type ";
		print_string (Type.stringOf exp_type);
		print_string " which doesn't match " ;
		print_string var;
		print_string " of type ";
		print_endline (Type.stringOf var_type)
	| Non_typed_exp	->
		print_endline "The expression has no type"
	

let illegal_class_name str loc =
  raise (Error (Illegal_class_name str,loc))

let illegal_inheritance str name loc =
  raise (Error (Illegal_inheritance (str,name),loc))

let used_class_name str loc =
  raise (Error (Used_class_name str,loc))

let used_meth_name str loc =
  raise (Error (Used_meth_name str,loc))

let used_att_name str loc =
  raise (Error (Used_att_name str,loc))

let inheritance_loop str loc =
  raise (Error (Inheritance_loop str,loc))

let non_existing_class str loc =
  raise (Error (Non_existing_class str,loc))

let non_existing_method class_type meth loc =
  raise (Error (Non_existing_method (class_type,meth),loc))

let non_existing_attribute class_type att loc =
  raise (Error (Non_existing_attribute (class_type,att),loc))

let args_error str id loc =
  raise (Error (Args_Error (str,id),loc))

let return_type_error str return_type exp_type loc =
  raise (Error (Return_Error (str,return_type,exp_type),loc))

let sig_error str loc_parent loc =
  raise (Error (Sig_Error (str,loc_parent),loc))

let incorrect_type type1 type2 loc =
	raise (Error (Incorrect_type (type1,type2),loc))

let incorrect_var_type var var_type exp_type loc =
	raise (Error (Incorrect_var_type (var,var_type,exp_type),loc))

let non_typed_exp loc =
  raise (Error (Non_typed_exp,loc))