open Env
open AST
open ExecError

(**
 * Type contenant les valeurs pour l'évaluation
 *)

type ev_value =
	| String of string
	| Int of int
	| Null
	| Boolean of bool
	| Object of Type.t * ev_value list

(**
 * Trouve une méthode dans une liste de nom préfixés
 * 
 * Prend le nom de la méthode à trouver
 * Prend la liste dans laquelle rechercher
 *
 * Renvoie le nom de la méthode trouvée
 *)

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
		begin
		match String.compare sub_string meth with
			| 0	-> h
			| _	-> find_name meth t
		end

(**
 * Récupère une méthode pour un objet donné
 * 
 * Prend les tables globales
 * Prend le type de l'objet pour lequel la méthode est recherchée
 * Prend le nom le nom de la méthode à trouver
 *
 * Renvoie la méthode trouvée
 *)

let get_method meth_table class_desc (val_type,values) meth =
	let parent::meths = find class_desc val_type in
	let meth_name = find_name meth meths in
	find meth_table meth_name

(**
 * Récupère l'index de position d'un nom recherché dans une liste
 * 
 * Prend l'accumulateur pour la position
 * Prend le nom à rechercher
 * Prend la liste
 *
 * Renvoie la position
 *)

let rec find_att_index acc str = function
	| []		-> unexpected_error Location.none
	| (name,expr)::t	-> 
		match String.compare name str with
			| 0	-> acc
			|	_	-> find_att_index (acc+1) str t

(**
 * Crée un environnement local à l'appel d'une fonction
 * 
 * Prend la liste des valeurs passées en argument
 * Prend la liste des arguments
 *
 * Renvoie l'environnement
 *)
	
let create_env value_list args =
	let env = initial() in
	List.fold_left2 (fun hash a b -> define hash a b) env args value_list

(**
 * Compare deux valeurs de même type
 * 
 * Prend les valeurs à comparer
 *
 * Renvoie un booléen résultat de la comparaison
 *)
	
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

(**
 * Compare deux objets
 * 
 * Prend les objets à comparer
 *
 * Renvoie un booléen résultat de la comparaison
 *)
	
and compare_object (t1,l1) (t2,l2) =
	match l1,l2 with
		| [],[]					-> true
		| h1::q1,[]			-> false
		| [],h2::q2			-> false
		| h1::q1,h2::q2	-> (compare_values h1 h2) && (compare_object (t1,q1) (t2,q2))

(**
 * Evalue les valeurs venant de l'Ast et renvoie la valeur 
 * correspondante pour l'évaluation
 *)
		
let evaluate_value = function
	| AST.String s 	-> String s
	| AST.Int i 		-> Int i
	| AST.Null 			-> Null
	| AST.Boolean b -> Boolean b
	
(**
 * Crée une valeur du type envoyé en argument
 *
 * Prend les tables globales de description
 * Prend le type de la valeur à créer
 *
 * Renvoie une valeur
 *)

let rec create_object meth_table class_desc object_desc obj_val =
	match Type.stringOf obj_val with
		| "String"	-> String ""
		| "Int"			-> Int 0
		| "Boolean"	-> Boolean true
		| _					->
			let att_list = find object_desc obj_val in
			Object (obj_val,evaluate_attributes obj_val meth_table class_desc object_desc (initial()) att_list)
(**
 * Evalue l'ensemble des attributs avec leurs valeurs par défaut lors de
 * l'initialisation d'un objet
 *
 * Prend en entrée les tables globales
 * Prend l'environnement local à l'initialisation de l'objet
 * Prend la liste des attributs à initialiser
 *
 * Renvoie la liste des attributs initialisés
 *)

and evaluate_attributes obj_val meth_table class_desc object_desc env = function
	| []		-> []
	| (name,expr)::t	->
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
					let meth_body,args = get_method meth_table class_desc (t,v) meth in
					let local_env = create_env value_list args in
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
							undefined_attribute s e.eloc
					in
					env,found
			end
		| AST.Assign (str,x) -> 
			let new_env,value_x = curry_evaluate x in
			let local_env = replace new_env str value_x in
			local_env,value_x
		| AST.Define (str,val_type,x,y)	->
			let new_env,value_x = curry_evaluate x in
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
			print_string "Result : ";
			let env,value = evaluate_expression meth_table class_desc object_desc (Object ((Type.fromString "Object"),[])) (initial()) exp in
			match value with
				| String s	-> print_endline s
				| Int i			-> print_endline (string_of_int i)
				| Null			-> print_endline "Null"
				| Boolean b	-> print_endline (string_of_bool b)
				| Object (t,att_list) -> print_endline "coucou"
