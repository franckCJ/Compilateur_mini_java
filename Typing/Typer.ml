open AST
open Type

let calc_bin_op = "sub"|"add"|"mul"|"div"|"mod"
let comp_bin_op = "gt"|"ge"|"lt"|"le"|"eq"|"neq"
let bool_bin_op = "and"|"or"

let equals_string s1 s2 =
	let comp = String.compare s1 s2 in
	match comp with
		| 0 -> true
		| _ -> false

let compare_type t1 t2 =
	String.compare (stringOf t1) (stringOf t2)

let compare_sig s1 s2 =
	String.compare (string_of_func s1) (string_of_func s2)

let find_type class_table class_name args_table local_table var_name =
	try
		Hashtbl.find local_table var_name
	with Not_Found ->
		try
			Hashtbl.find args_table var_name
		with Not_Found ->
			try
				let (att_table,meth_table) = Hashtbl.find class_table class_name in
				Hashtbl.find att_table var_name
			with Not_Found ->
				print_endline "Error, Attribute not found"; fromString ""


(*********** Vérification des classes, méthodes et attributs **********
 *********** et création des tables globales 								 **********) 
let protected_classes = "Object" | "Int" | "String" | "Boolean"

let rec filter_meth_by_sig meth_list meth_table = function
	| []		-> meth_list
	| (name,sign)::t as curr_list ->
		try
			let (local_sign,loc) = Hashtbl.find meth_table name in
			let comp = compare_sig sign local_sign in
			match comp with
				| 0	-> verify_meth_sig meth_list meth_table t
				| _ -> print_endline "Erreur, signatures non compatibles"
		with Not_Found ->
			verify_meth_sig (name,sign)::meth_list meth_table t
			
let rec check_extends_cycle class_table meth_list class_list key (parent,atts,meths,loc) =
	match key with
		| protected_classes		-> ()
		| str 								->
			match (List.mem str class_list) with
				| true 	-> print_endline "Erreur, bouclage d'heritage"
				| false ->
					let parent_name = elem_of parent in
					try
						let pparent,patts,pmeths,ploc = Hashtbl.find class_table parent_name in
						let remaining_meths = filter_meth_by_sig [] pmeths meth_list in
						check_extends_cycle class_table remaining_meths str::class_list parent_name (pparent,patts,pmeths,ploc)
					with Not_Found ->
						print_endline "Erreur, le parent n'existe pas"

let rec find_att att_table att_list =
	match att_list with
		| [] 		-> att_table
		| h::t	-> 
			let name_used = Hashtbl.mem att_table h.aname in
			match name_used with
				| true	-> print_endline "Erreur, deux attributs de même nom"
				| false ->
					Hashtbl.add att_table h.aname (Located.elem_of h.atype,h.aloc);
					find_att att_table t

let create_sig m =
	let name_list,type_list = List.split m.margstype in
	let args_types = List.map Located.elem_of type_list in
	let return_type = Located.elem_of m.mreturntype in
	create_func arg_type return_type

let rec find_method meth_table meth_list =
	match meth_list with
		| [] 		-> meth_table
		| h::t	->
			let name_used = Hashtbl.mem meth_table h.mname in
			match name_used with
				| true	-> print_endline "Erreur, deux méthodes de même nom"
				| false ->
					Hashtbl.add meth_table h.mname (create_sig h,h.mloc);
					find_method meth_table t

(* Vérifie si le nom de la classe ne correspond pas à Int, String, Boolean, Null,
	Object => classes protégées *)
let rec find_classes class_table cl =
	match cl with
		| [] 		-> class_table
		| h::t	-> 
			let name_used = Hashtbl.mem class_table h.cname in (*créer un type used_classes pour les classes déjà vues*)
			match name_used with
				| true	-> print_endline "Erreur, deux classes définies de même nom"
				| false ->
					match h.cname with
						| protected_classes	-> print_endline "Erreur, nom de classe protégé"
						| _									->
							let att_table = find_att (Hashtbl.create 0) h.cattributes in
							let meth_table = find_method (Hashtbl.create 0) h.cmethods in
							Hashtbl.add class_table h.cname (h.cparent,h.att_table,meth_table,h.cloc);
							find_classes class_table t


(*********** Détermination des correspondances des types     **********)
let value_type v = function
	| String of string 	-> fromString "String"
	| Int of int 				-> fromString "Int"
	| Null 							-> fromString "Null"
	| Boolean of bool 	-> fromString "Boolean"

let call_type op first_type type_list =
	match op with
		| "not" 			-> first_type
		| "neg" 			-> first_type
		| calc_bin_op	->
			match compare_type first_type type_list[0] with
				| 0 -> first_type
				| _ -> print_endline "erreur de type"; fromString ""
		| comp_bin_op	->
			match compare_type first_type type_list[0] with
				| 0 -> fromString "Boolean"
				| _ -> print_endline "erreur de type"; fromString ""
		| bool_bin_op	->
			let first_comp = String.compare (stringOf firstType) "Boolean" in
			let second_comp = String.compare (stringOf type_list[0]) "Boolean" in
			match first_comp second_comp with
				| (0,0) -> fromString "Boolean"
				| _ 		-> print_endline "erreur de type"; fromString ""
		| _						-> (*Rechercher la méthode op dans la classe m=first_type*) fromString ""

let rec type_expression class_table class_name args_table local_table e =
	(*faire la curification de la fonction*)
	let exp = { 
		e with
		etype =
			match e.edesc with
				| New nval 								-> Some (elem_of nval)
				| Seq x y 								->
					type_expression x;
					type_expression y;
					Some ""
				| Call x str lst 					->
					let type_x = type_expression x in
					let type_list = List.map type_expression lst in
					Some (call_type str type_x type_list)
				| If x y  								->
					let cond 		= String.compare (type_expression x).etype "Boolean" in
					let type_y 	= (type_expression y).etype in
					let type_z 	= (type_expression z).etype in
					let comp		= String.compare type_y type_z in
						match (cond,comp) with
							| (0,0) -> Some type_y
							| (0,_) -> print_endline "erreur de type"; Some ""
							| (_,0)	-> print_endline "erreur de type"; Some ""
				| Val v 									-> Some (value_type v)
				|	Var str 								-> 
					match str with
						| "this"	->
						| _				-> Some (find_type class_table class_name args_table local_table str)
				| Assign str x 						->
					let var_type = find_type class_table class_name args_table local_table str in
					let typed_exp = type_expression x in
					match (var_type,x.etype) with
						| Some t1, Some t2 ->
							match (compare_type t1 t2) with
								| 0 -> Some t1
								| _ -> print_endline "Erreur de type"; Some ""
						| _ -> print_endline "Erreur de type"; Some "" (*Possibilité de différencier les cas*)
				| Define str val_type x y	->
					let typed_x = type_expression class_table class_name args_table local_table x in
					match typed_x.etype with
						| None -> print_endline "Erreur de type"
						| Some t -> 
							match (compare_type t (elem_of val_type)) with
							| 0 ->
								Hashtbl.add local_table str val_type;
								type_expression class_table class_name args_table local_table y;
								Hashtbl.remove local_table str;
								Some (elem_of val_type)
							| _ -> print_endline "Erreur de type"
				| Cast new_type x 				-> type_expression x; Some (elem_of new_type)
				| Instanceof x t 					-> type_expression x; Some (fromString "Boolean")
	} in
	exp

let type_attribute class_table class_name a = 
	match a.adefault with
		|	None 		-> a
		|	Some e 	->
			let typed_exp = type_expression e in
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

let type_method class_table class_name m =
	let arg_table = Hashtbl.create (List.length m.margstype);
	List.iter (fun(name,arg_type) -> Hashtbl.add arg_table name (elem_of arg_type)) method_args;
	let typed_body = type_expression class_table class_name arg_table (Hashtbl.create 0) m.mbody in
	let me = { 
		m with
			mbody = typed_body;
	} in
	match typed_body.etype with
		| None -> print_endline "erreur de type"; m
		| Some t ->
			let comp = compare_type t (Located.elem_of m.mreturntype) in
			match comp with
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

(* Vérifier le cyclage des extends *)
(* Ajouter la classe mère dans la table des classes *)
let type_program (cl,e_op) = 
	let class_table = find_classes (Hashtbl.create 0);
	Hashtbl.iter (check_extends_cycle class_table []) class_table;
	let typed_cl = List.map type_class class_table cl in
	match e_op with
		| None 		-> typed_cl,None
		| Some e 	-> typed_cl,(Some (type_expression e))
