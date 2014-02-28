open Env
open AST
open ExecError

type ev_value =
	| String of string
	| Int of int
	| Null
	| Boolean of bool
	| Object of Type.t * ev_value list

let rec find_name meth = function
	| []		-> ""
	| h::t	->
		let beg_index =
		try
			 (String.index h '-')+1
		with Not_found	-> 0
		in 
		let end_index = String.length h in
		let sub_string = String.sub h beg_index (end_index-beg_index) in
		print_endline sub_string;
		begin
		match String.compare sub_string meth with
			| 0	-> h
			| _	-> find_name meth t
		end

let get_method meth_table class_desc (val_type,values) meth =
	let parent::meths = find class_desc val_type in
	let meth_name = find_name meth meths in
	print_endline meth_name;
	find meth_table meth_name

let rec find_att_index acc str = function
	| []		-> unexpected_error Location.none
	| (name,expr)::t	-> 
		match String.compare name str with
			| 0	-> acc
			|	_	-> find_att_index (acc+1) str t

let create_env value_list args =
	let env = initial() in
	print_endline "*************";
	List.iter (fun a -> print_endline a) args;
	List.fold_left2 (fun hash a b -> define hash a b) env args value_list

let rec compare_values val1 val2 =
	match val1,val2 with
		| Int i1, Int i2					-> i1 == i2
		| String s1, String s2		->
			begin
			match String.compare s1 s2 with
				| 0	-> true
				| _	-> false
			end
		| Boolean b1, Boolean b2						-> (b1 || not(b2)) && (not(b1) || b2)
		| Null, _														-> false
		| _, Null														-> false
		| Object (t1,l1), Object (t2,l2)		-> compare_object (t1,l1) (t2,l2)
		| _																	-> unexpected_error Location.none

and compare_object (t1,l1) (t2,l2) =
	match l1,l2 with
		| [],[]					-> true
		| h1::q1,[]			-> false
		| [],h2::q2			-> false
		| h1::q1,h2::q2	-> (compare_values h1 h2) && (compare_object (t1,q1) (t2,q2))
			
let evaluate_value = function
	| AST.String s 	-> String s
	| AST.Int i 		-> Int i
	| AST.Null 			-> Null
	| AST.Boolean b -> Boolean b

let rec create_object meth_table class_desc object_desc obj_val =
	match Type.stringOf obj_val with
		| "String"	-> String ""
		| "Int"			-> Int 0
		| "Boolean"	-> Boolean true
		| _					->
			let att_list = find object_desc obj_val in
			Object (obj_val,evaluate_attributes obj_val meth_table class_desc object_desc (initial()) att_list)

and evaluate_attributes obj_val meth_table class_desc object_desc env = function
	| []		-> []
	| (name,expr)::t	->
		print_string "Initialisation d'attribut : ";
		print_endline name;
		match expr with
			| None				-> 
				let new_env = define env name Null in
				Null::(evaluate_attributes obj_val meth_table class_desc object_desc new_env t)
			| Some etype	->
				let env,ev_expr = evaluate_expression meth_table class_desc object_desc (Object (obj_val,[])) env etype in
				let new_env = define env name ev_expr in
				ev_expr::(evaluate_attributes obj_val meth_table class_desc object_desc new_env t)

(**
 * Evalue l'appel à une fonction
 *
 * Prend en entrée les tables globales
 * Prend un environnement
 * Prend l'opérateur
 * Prend la valeur primaire
 * Prend liste des autres valeurs
 *
 * Renvoie la valeur évaluée
 *)

and evaluate_call meth_table class_desc object_desc env op value value_list =
	match op with
		| "not"	->
			begin
			match value with
				| Boolean b	-> Boolean (not(b))
				| _					-> unexpected_error Location.none
			end
		| "neg" ->
			begin
			match value with
				| Int i	-> Int (-i)
				| _			-> unexpected_error Location.none
			end
		| "add" ->
			begin
			match value,List.hd value_list with
				| Int i1, Int i2	-> Int (i1 + i2)
				| _								-> unexpected_error Location.none
			end
		| "sub" ->
			begin
			match value,List.hd value_list with
				| Int i1, Int i2	-> Int (i1 - i2)
				| _								-> unexpected_error Location.none
			end
		| "mul" ->
			begin
			match value,List.hd value_list with
				| Int i1, Int i2	-> Int (i1 * i2)
				| _								-> unexpected_error Location.none
			end
		| "div" ->
			begin
			match value,List.hd value_list with
				| Int i1, Int i2	-> Int (i1 / i2)
				| _								-> unexpected_error Location.none
			end
		| "mod"	->
			begin
			match value,List.hd value_list with
				| Int i1, Int i2	-> Int (i1 mod i2)
				| _								-> unexpected_error Location.none
			end
		| "gt" 	->
			begin
			match value,List.hd value_list with
				| Int i1, Int i2	-> Boolean (i1 > i2)
				| _								-> unexpected_error Location.none
			end
		| "ge"	->
			begin
			match value,List.hd value_list with
				| Int i1, Int i2	-> Boolean (i1 >= i2)
				| _								-> unexpected_error Location.none
			end
		| "lt"	->
			begin
			match value,List.hd value_list with
				| Int i1, Int i2	-> Boolean (i1 < i2)
				| _								-> unexpected_error Location.none
			end
		| "le"	->
			begin
			match value,List.hd value_list with
				| Int i1, Int i2	-> Boolean (i1 <= i2)
				| _								-> unexpected_error Location.none
			end
		| "eq"	-> Boolean (compare_values value (List.hd value_list))
		| "neq"	-> Boolean (not(compare_values value (List.hd value_list)))
		| "and" ->
			begin
			match value,List.hd value_list with
				| Boolean i1, Boolean i2	-> Boolean (i1 && i2)
				| _												-> unexpected_error Location.none
			end
		| "or"	->
			begin
			match value,List.hd value_list with
				| Boolean i1, Boolean i2	-> Boolean (i1 || i2)
				| _												-> unexpected_error Location.none
			end
		| _ as meth		->
			begin
			match value with
				| Object (t,v)	->
					print_endline meth;
					let meth_body,args = get_method meth_table class_desc (t,v) meth in
					let local_env = create_env value_list args in
					print_endline (Type.stringOf t);
					print_endline (string_of_int (List.length v));
					let env,eval_val = evaluate_expression meth_table class_desc object_desc value local_env meth_body in
					eval_val
				| _ -> unexpected_error Location.none
			end

(**
 * Evalue une expression selon la valeur de son descripteur
 *
 * Prend en entrée les tables globales
 * Prend un environnement
 * Prend l'expression à évaluer
 *
 * Retourne le couple environnement_modifié,valeur
 *)

and evaluate_expression meth_table class_desc object_desc env_object env e =
	let curry_evaluate = evaluate_expression meth_table class_desc object_desc env_object env in
	match e.edesc with
		| AST.New nval 	-> env,create_object meth_table class_desc object_desc (Located.elem_of nval)
		| AST.Seq (x,y)	->
			let new_env,value_x = curry_evaluate x in
			evaluate_expression meth_table class_desc object_desc env_object new_env y
		| AST.Call (x,str,lst)	->
			let new_env,value_x = (curry_evaluate x) in
			let env_list,value_list = List.split (List.map curry_evaluate lst) in
			new_env,evaluate_call meth_table class_desc object_desc new_env str value_x value_list
		| AST.If (x,y,z)	-> 
			let new_env,value_x = curry_evaluate x in
			begin
			match value_x with 
				| Boolean true 	-> evaluate_expression meth_table class_desc object_desc env_object new_env y
				| _ 						-> evaluate_expression meth_table class_desc object_desc env_object new_env z
			end
		| AST.Val v 	-> env,evaluate_value v
		|	AST.Var str -> 
			begin
			match str with
				| "this"	-> env,env_object
				| _	as s	-> 
					let found = 
					try
						find env s
					with Not_found	-> 
						let Object (obj_type,value_list) = env_object in
						let att_list = find object_desc obj_type in
						let index = find_att_index 0 str att_list in
						try
							List.nth value_list index
						with Failure "nth"	->
							undefined_attribute s loc
					in
					env,found
			end
		| AST.Assign (str,x) -> 
			let new_env,value_x = curry_evaluate x in
			let local_env = replace new_env str value_x in
			local_env,value_x
		| AST.Define (str,val_type,x,y)	->
			print_endline "ici";
			let new_env,value_x = curry_evaluate x in
			print_endline "initialisation faire";
			let local_env = define new_env str value_x in
			let new_env,eval_value = evaluate_expression meth_table class_desc object_desc env_object local_env y in
			env,eval_value
		| AST.Cast (new_type,x)	-> curry_evaluate x
		| AST.Instanceof (x,t)	-> 
			let result = Typer.compare_type (Typer.get_type x) (Located.elem_of t) in
			env,Boolean (result)

(**
 * Evalue l'expression de plus haut niveau du programme
 * les tables a jour
 *
 * Prend en entrée les tables de méthodes, de descripteurs de classe et de
 * descripteur d'objet
 * Prend l'expression à évaluer
 *
 * Imprime la valeur retournée par l'évaluation
 *)

let evaluate_program ((meth_table,class_desc,object_desc),e_op) = 
	match e_op with
		| None 			-> print_endline "Nothing to do"
		| Some exp	->
			let env,value = evaluate_expression meth_table class_desc object_desc (Object ((Type.fromString "Object"),[])) (initial()) exp in
			match value with
				| String s	-> print_endline s
				| Int i			-> print_endline (string_of_int i)
				| Null			-> print_endline "Null"
				| Boolean b	-> print_endline (string_of_bool b)
				| Object (t,att_list) -> print_endline "coucou"
