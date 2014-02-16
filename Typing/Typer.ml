open AST
open Type
open TypeError

let protected_classes = Str.regexp "Object \\| Int \\| String \\| Boolean"
let calc_bin_op = Str.regexp "sub \\| add \\| mul \\| div \\| mod"
let comp_bin_op = Str.regexp "gt \\| ge \\| lt \\| le \\| eq \\| neq"
let bool_bin_op = Str.regexp "and \\| or"

let equals_string s1 s2 =
	let comp = String.compare s1 s2 in
	match comp with
		| 0 -> true
		| _ -> false

let compare_type t1 t2 =
	String.compare (stringOf t1) (stringOf t2)

let compare_sig s1 s2 =
	String.compare (string_of_func s1) (string_of_func s2)

let get_type = function
	| None		-> print_endline "Problème de typage"; fromString ""
	| Some t	-> t

let exists_type class_table = function
	| protected_classes -> true
	| _ as str 					-> Hashtbl.mem class_table str


let rec is_parent class_table t1 t2 =
	try
		let parent,atts,meths,loc = Hashtbl.find class_table t2 in
		match Located.elem_of parent with
			| t1 								-> true
			| protected_classes -> false
			| _ as p 						-> is_parent class_table t1 p
	with Not_found ->
		print_endline	("Error, le type " ^ (stringOf t2) ^ " est inconnu"); false

let rec has_common_parent class_table t1 t2 =
	match is_parent class_table t1 t2 with
		| true 	-> true 
		| false	->
			try
				let parent,atts,meths,loc = Hashtbl.find class_table t1 in
				has_common_parent class_table (Located.elem_of parent) t2
			with Not_found ->
				print_endline ("Error, le type " ^ (stringOf t1) ^ " est inconnu"); false
	
let find_type class_table class_name args_table local_table var_name =
	try
		Hashtbl.find local_table var_name
	with Not_found ->
		try
			Hashtbl.find args_table var_name
		with Not_found ->
			try
				let (parent,att_table,meth_table,loc) = Hashtbl.find class_table class_name in
				let att_type,loc = Hashtbl.find att_table var_name in
				att_type
			with Not_found ->
				print_endline "Error, Attribute not found"; fromString ""

let rec check_args_type class_table args = function
	| [] ->
		match args with
			| [] 	-> ()
			| _		-> print_endline "Pas assez d'arguments dans la méthode"
	| h::t ->
		match args with
			| []			-> print_endline "Trop d'arguments dans la méthode"
			| ah::at	->
				match is_parent class_table ah h with
					| true	-> check_args_type class_table at t
					| false	-> print_endline "Les arguments n'ont pas le bon type"

let rec find_meth_type class_table meth_name type_list class_name = 
	try
		let parent,atts,meths,loc = Hashtbl.find class_table class_name in
			try
				let sign,loc = Hashtbl.find meths meth_name in
				let args = get_args_list sign in
				let res = get_result_type sign in
				check_args_type class_table args type_list;
				res
			with Not_found ->
				match parent with
					| protected_classes -> print_endline "La méthode n'est pas défini dans la classe"; fromString ""
					| _ 								-> find_meth_type class_table meth_name type_list (Located.elem_of parent)
		with Not_found ->
			print_endline "Le type de la classe appelée n'existe pas"; fromString ""

(*********** Vérification des classes, méthodes et attributs **********
 *********** et création des tables globales 								 **********) 
let rec filter_meth_by_sig meth_list meth_table = function
	| []		-> meth_list
	| (name,sign)::t as curr_list ->
		try
			let (local_sign,loc) = Hashtbl.find meth_table name in
			let comp = compare_sig sign local_sign in
			match comp with
				| 0	-> filter_meth_by_sig meth_list meth_table t
				| _ -> print_endline "Erreur, signatures non compatibles"; []
		with Not_found ->
			filter_meth_by_sig ((name,sign)::meth_list) meth_table t
			
let rec check_extends_cycle class_table meth_list class_list key (parent,atts,meths,loc) =
	match key with
		| protected_classes		-> ()
		| str 								->
			match (List.mem str class_list) with
				| true 	-> print_endline "Erreur, bouclage d'heritage"
				| false ->
					let parent_name = Located.elem_of parent in
					try
						let pparent,patts,pmeths,ploc = Hashtbl.find class_table parent_name in
						let remaining_meths = filter_meth_by_sig [] pmeths meth_list in
						check_extends_cycle class_table remaining_meths (str::class_list) parent_name (pparent,patts,pmeths,ploc)
					with Not_found ->
						print_endline "Erreur, le parent n'existe pas"

let rec find_att att_table att_list =
	match att_list with
		| [] 		-> att_table
		| h::t	-> 
			let name_used = Hashtbl.mem att_table h.aname in
			match name_used with
				| true	-> print_endline "Erreur, deux attributs de même nom"; att_table
				| false ->
					Hashtbl.add att_table h.aname (Located.elem_of h.atype,h.aloc);
					find_att att_table t

let create_sig m =
	let name_list,type_list = List.split m.margstype in
	let args_type = List.map Located.elem_of type_list in
	let return_type = Located.elem_of m.mreturntype in
	create_func (args_type,return_type)

let rec find_method meth_table meth_list =
	match meth_list with
		| [] 		-> meth_table
		| h::t	->
			let name_used = Hashtbl.mem meth_table h.mname in
			match name_used with
				| true	-> print_endline "Erreur, deux méthodes de même nom"; meth_table
				| false ->
					Hashtbl.add meth_table h.mname (create_sig h,h.mloc);
					find_method meth_table t

(* Vérifie si le nom de la classe ne correspond pas à Int, String, Boolean, Null,
	Object => classes protégées *)
let rec find_classes class_table cl =
	match cl with
		| [] 		-> class_table
		| h::t	-> 
			let name_used = Hashtbl.mem class_table h.cname in
			match name_used with
				| true	-> print_endline "Erreur, deux classes définies de même nom"; class_table
				| false ->
					match h.cname with
						| protected_classes	-> print_endline "Erreur, nom de classe protégé"; class_table
						| _									->
							let att_table = find_att (Hashtbl.create 0) h.cattributes in
							let meth_table = find_method (Hashtbl.create 0) h.cmethods in
							Hashtbl.add class_table h.cname (h.cparent,att_table,meth_table,h.cloc);
							find_classes class_table t


(*********** Détermination des correspondances des types     **********)
let value_type = function
	| String s 	-> fromString "String"
	| Int i 		-> fromString "Int"
	| Null 			-> fromString "Null"
	| Boolean b -> fromString "Boolean"

let call_type class_table op first_typed typed_list =
	let first_type = get_type (first_typed.etype) in
	let type_list = List.map (function hd -> get_type hd.etype) typed_list in
	match op with
		| "not" 			-> first_type
		| "neg" 			-> first_type
		| calc_bin_op	->
			match (compare_type first_type (List.hd type_list)) with
				| 0 -> first_type
				| _ -> print_endline "erreur de type"; fromString ""
		| comp_bin_op	->
			match (compare_type first_type (List.hd type_list)) with
				| 0 -> fromString "Boolean"
				| _ -> print_endline "erreur de type"; fromString ""
		| bool_bin_op	->
			let first_comp = compare_type first_type (fromString "Boolean") in
			let second_comp = compare_type (List.hd type_list) (fromString "Boolean") in
			match (first_comp,second_comp) with
				| (0,0) -> fromString "Boolean"
				| _ 		-> print_endline "erreur de type"; fromString ""
		| _ as meth		-> find_meth_type class_table meth type_list first_type

let rec type_expression class_table class_name args_table local_table e =
	let curry_type_expression = type_expression class_table class_name args_table local_table in
	let exp = { 
		e with
		etype =
			match e.edesc with
				| New nval 								-> Some (Located.elem_of nval)
				| Seq (x,y)								->
					curry_type_expression x;
					(curry_type_expression y).etype
				| Call (x,str,lst)				->
					let typed_x = (curry_type_expression x) in
					let typed_list = List.map curry_type_expression lst in
					Some (call_type class_table str typed_x typed_list)
				| If (x,y,z) 								->
					let cond 		= compare_type (get_type (curry_type_expression x).etype) (fromString "Boolean") in
					let type_y 	= get_type (curry_type_expression y).etype in 
					let type_z 	= get_type (curry_type_expression z).etype in
					let comp		= has_common_parent class_table type_y type_z in
						(match (cond,comp) with
							| (0,true) 	-> Some type_y
							| (0,_) 		-> print_endline "erreur de type"; Some (fromString "")
							| (_,true)	-> print_endline "erreur de type"; Some (fromString "")
							| (_,_)			-> print_endline "erreur de type"; Some (fromString "")
						)
				| Val v 									-> Some (value_type v)
				|	Var str 								-> 
					(match str with
						| "this"	-> Some (fromString class_name)
						| _				-> Some (fromString (find_type class_table class_name args_table local_table str))
					)
				| Assign (str,x)					-> 
					let var_type = find_type class_table class_name args_table local_table str in
					let typed_exp = curry_type_expression x in
					(match (var_type,x.etype) with
						| Some t1, Some t2 ->
							(match (is_parent t1 t2) with
								| true 	-> Some t1
								| _ 		-> print_endline "Erreur de type"; Some ""
							)
						| _ -> print_endline "Erreur de type"; Some "" (*Possibilité de différencier les cas*)
					)
				| Define (str,val_type,x,y)	->
					(match exists_type class_table val_type with
						| false	-> print_endline "Le type utilisé n'est pas défini"
						| true	->
							let typed_x = curry_type_expression x in
							(match typed_x.etype with
								| None -> print_endline "Erreur de type"
								| Some t -> 
									(match (is_parent (Located.elem_of val_type) t) with
										| true	->
											Hashtbl.add local_table str val_type;
											type_expression class_table class_name args_table local_table y;
											Hashtbl.remove local_table str;
											Some (Located.elem_of val_type)
										| _ 		-> print_endline "Erreur de type"
									)
							)
					)
				| Cast (new_type,x)				-> curry_type_expression x; Some (Located.elem_of new_type)
				| Instanceof (x,t)				-> curry_type_expression x; Some (fromString "Boolean")
	} in
	exp

let type_attribute class_table class_name a = 
	let correct_type = exists_type class_table a.atype in
	match correct_type,a.adefault with
		| false,_	-> print_endline "Le type utilisé n'existe pas"; a
		|	_,None 		-> a
		|	_,Some e 	->
			let typed_exp = type_expression class_table class_name args_table (Hashtbl.create 0) e in
			let at = { 
				a with
				adefault = Some typed_exp;
			} in				
			match typed_exp.etype with
				| None 		-> print_endline "erreur de type"; a
				| Some t 	->
					let comp = compare_type t (Located.elem_of a.atype) in
					match comp with
						| 0 -> at
						|	_ -> print_endline "erreur de type"; a

(*verifier les types d'entrée et de retour existant*)
let type_method class_table class_name m =
	let arg_table = Hashtbl.create (List.length m.margstype) in
	List.iter (fun(name,arg_type) -> Hashtbl.add arg_table name (Located.elem_of arg_type)) m.margstype;
	let typed_body = type_expression class_table class_name arg_table (Hashtbl.create 0) m.mbody in
	let me = { 
		m with
			mbody = typed_body;
	} in
	match typed_body.etype with
		| None -> print_endline "erreur de type"; m
		| Some t ->
			match compare_type t (Located.elem_of m.mreturntype) with
				| 0 -> me 
				| _ -> print_endline "erreur de type"; m

let type_class class_table c =
	let typed_attr = (List.map type_attribute class_table c.cname c.cattributes) in
	let typed_meth = (List.map type_method class_table c.cname c.cmethods) in
	let cl = { 
		c with
			cattributes = typed_attr;
			cmethods 		= typed_meth;
	} in
	cl

let type_program (cl,e_op) = 
	let class_table = find_classes (Hashtbl.create 0) in
	Hashtbl.iter (check_extends_cycle class_table []) class_table;
	let typed_cl = List.map type_class class_table cl in
	match e_op with
		| None 		-> typed_cl,None
		| Some e 	-> typed_cl,(Some (type_expression e))
