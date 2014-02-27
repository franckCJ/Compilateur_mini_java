open Env

type ev_value =
	| String of string
	| Int of int
	| Null
	| Boolean of bool
	| Object of Type.t * ev_value list

let rec find_name meth = function
	| []		-> ""
	| h::t	->
		let beg_index = String.index h '-' in
		let end_index = (String.length h)-1 in
		begin
		match String.sub h beg_index end_index with
			| meth	-> h
			| _			-> find_name meth t
		end

let create_object meth_table class_desc object_desc obj_val =
	match Type.stringOf obj_val with
		| "String"	-> String ""
		| "Int"			-> Int 0
		| "Boolean"	-> Boolean true
		| _					->
			let att_list = find object_desc obj_val in
			Object (obj_val,evaluate_attributes meth_table class_desc object_desc att_list)

let get_method meth_table class_desc (val_type,values) meth =
	let parent::meths = find class_desc val_type in
	let meth_name = find_name meth meths in
	find meth_table meth_name

let create_env value_list args =
	let env = initial() in
	List.fold_left2 (fun env a b -> define env a b) args value_list
			
let rec evaluate_attributes obj_val meth_table class_desc object_desc = function
	| []		-> []
	| h::t	->
		match h.adefault with
			| None				-> Null::(evaluate_attributes obj_val meth_table class_desc object_desc t)
			| Some etype	->
				let ev_expr = evaluate_expression meth_table class_desc object_desc (Object (obj_val,[])) initial() etype in
				ev_expr::(evaluate_attributes obj_val meth_table class_desc object_desc t)

let evaluate_value = function
	| AST.String s 	-> String s
	| AST.Int i 		-> Int i
	| AST.Null 			-> Null
	| AST.Boolean b -> Boolean b

let evaluate_call env op value value_list =
	match op with
		| "not"	-> not(value)
		| "neg" -> -value
		| "add" -> value + (List.hd type_list)
		| "sub" -> value - (List.hd type_list)
		| "mul" -> value * (List.hd type_list)
		| "div" -> value / (List.hd type_list)
		| "mod"	-> value mod (List.hd type_list)
		| "gt" 	-> value > (List.hd type_list)
		| "ge"	-> value >= (List.hd type_list)
		| "lt"	-> value < (List.hd type_list)
		| "le"	-> value <= (List.hd type_list)
		| "eq"	-> value = (List.hd type_list)
		| "neq"	-> value != (List.hd type_list)
		| "and" -> value && (List.hd type_list)
		| "or"	-> value || (List.hd type_list)
		| _ as meth		->
			let meth_body,args = get_method value meth in
			let local_env = create_env value_list args in
			evaluate_expression meth_table class_desc object_desc value local_env meth_body

let rec evaluate_expression meth_table class_desc object_desc env_object env e =
	let curry_evaluate = evaluate_expression meth_table class_desc object_desc env_object env in
	match e.edesc with
		| AST.New nval 	-> env,create_object meth_table class_desc object_desc nval
		| AST.Seq (x,y)	->
			let new_env,value_x = curry_evaluate x in
			evaluate_expression meth_table class_desc object_desc env_object new_env y
		| AST.Call (x,str,lst)	->
			let value_x = (curry_evaluate x) in
			let value_list = List.map curry_evaluate lst in
			evaluate_call env str value_x value_list
		| AST.If (x,y,z)	-> 
			begin
			match curry_evaluate x with 
				| Boolean true 	-> curry_evaluate y
				| _ 						-> curry_evaluate z
			end
		| AST.Val v 	-> env,evaluate_value v
		|	AST.Var str -> 
			begin
			match str with
				| "this"	-> env,env_object
				| _	as s	-> env,find env s
			end
		| AST.Assign (str,x) -> 
			let value_x = curry_evaluate x in
			let local_env = replace env str x in
			local_env,Null
		| AST.Define (str,val_type,x,y)	->
			let value_x = curry_evaluate x in
			let local_env = define env str value_x in
			let new_env,eval_value = evaluate_expression meth_table class_desc object_desc env_object local_env y in
			env,eval_value
		| AST.Cast (new_type,x)	-> curry_evaluate x
		| AST.Instanceof (x,t)	-> 
			let result = x.etype in
			env,Boolean result

let evaluate_program ((in_meth_table,in_class_desc,in_object_desc),e_op) = 
	meth_table := copy in_meth_table;
	class_desc := copy in_class_desc;
	object_desc := copy in_object_desc;
	match evaluate_expression meth_table class_desc object_desc (Object ((fromString "Object"),[])) initial() e_op with
		| String s	-> print_endline s
		| Int i			-> print_endline (string_of_int i)
		| Null			-> print_endline "Null"
		| Boolean b	-> print_enline (string_of_bool b)
		| Object (t,att_list) -> List.iter print_endline att_list
