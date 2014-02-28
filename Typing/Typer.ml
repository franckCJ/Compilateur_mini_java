open AST
open Type
open TypeError;;

let protected_classes = [fromString "Object" ; fromString "Int" ; fromString "String" ; fromString "Boolean"]

let value_type = function
	| String s 	-> fromString "String"
	| Int i 		-> fromString "Int"
	| Null 			-> fromString "Null"
	| Boolean b -> fromString "Boolean"

let class_table = Hashtbl.create 0

let compare_type t1 t2 =
	match String.compare (stringOf t1) (stringOf t2) with
		| 0 -> true
		| _ -> false 

let compare_sig s1 s2 =
	String.compare (string_of_func s1) (string_of_func s2)

(* Construit la signature d'une methode *)
let create_sig m =
	let name_list,type_list = List.split m.margstype in
	let args_type = List.map Located.elem_of type_list in
	let return_type = Located.elem_of m.mreturntype in
	create_func (args_type,return_type)

(* Retourne le type d'une expression *)
let get_type exp =
	match exp.etype with
		| None		-> non_typed_exp exp.eloc
		| Some t	-> t

let exists_type value_type = 
	match List.mem value_type protected_classes with
		| true -> true
		| _ 	 -> Hashtbl.mem class_table value_type

(* Verifie si le type t1 est un parent du type t2 *)
let rec is_parent t1 t2 =
	let type_t1 = Located.elem_of t1 in
	let type_t2 = Located.elem_of t2 in
	match compare_type type_t1 type_t2 with
		| true -> true
		| _ 	 -> 
			begin
			match List.mem type_t2 protected_classes with	
				| true -> incorrect_type type_t2 type_t1 (Located.loc_of t2)
				| _ 	 -> 
					try
						let parent,atts,meths,loc = Hashtbl.find class_table type_t2 in
						let type_parent = Located.elem_of parent in
						match List.mem type_parent protected_classes with 
							|	true -> false
							| _ 	->
								begin
								match type_t1 with
									| type_parent	-> true
									| _  					-> is_parent t1 parent
								end
					with Not_found ->
						non_existing_class type_t2 (Located.loc_of t2)
			end

(* Renvoie le parent commun des types t1 et t2 *)
let rec common_parent t1 t2 =
	let type_t1 = Located.elem_of t1 in
	match is_parent t1 t2 with
		| true 	-> type_t1
		| false	->
			try
				let parent,atts,meths,loc = Hashtbl.find class_table type_t1 in
				common_parent parent t2
			with Not_found ->
				non_existing_class type_t1 (Located.loc_of t1)

let has_common_parent t1 t2 =
	let type_t1 = Located.elem_of t1 in
	match stringOf type_t1 with
		| "Int" | "String" | "Boolean" -> (compare_type type_t1 (Located.elem_of t2)), type_t1
		| _ -> true,(common_parent t1 t2)

(**
 * Renvoie le type d'une variable en cherchant successivement
 * parmi les variables locales, puis parmi les arguments
 * et enfin parmi les attributs de la classe et de ses parents
 *)
let rec find_att class_name var_loc var_name =
	match List.mem class_name protected_classes with 
		| true	-> non_existing_attribute class_name var_name var_loc
		| _			->
			let (parent,att_table,meth_table,loc) = Hashtbl.find class_table class_name in
			try
				let att_type,loc = Hashtbl.find att_table var_name in
				att_type
			with Not_found ->
				find_att (Located.elem_of parent) var_loc var_name

let find_type class_name args_table local_table var_loc var_name =
	try
		Hashtbl.find local_table var_name
	with Not_found ->
		try
			Hashtbl.find args_table var_name
		with Not_found ->
			find_att class_name var_loc var_name

(** 
 * Verifie que les arguments passés dans un appel de methode
 * correspondent en nombre et en type à ceux déclare
 * en respectant l'ordre
 *)
let rec check_args_type meth_name loc args = function
	| [] ->
		begin
		match args with
			| [] 	-> ()
			| _		-> args_error meth_name (-1) loc
		end
	| h::t ->
		begin
		match args with
			| []			-> args_error meth_name 1 h.eloc
			| ah::at	->
				let ref_arg = Located.mk_elem ah h.eloc in
				let used_arg = Located.mk_elem (get_type h) h.eloc in
				begin
				match is_parent ref_arg used_arg with
					| true	-> check_args_type meth_name h.eloc at t
					| false	-> args_error meth_name 0 h.eloc
				end
		end

(**
 * Verifie que la methode a ete declaree dans la classe correspondante
 * Renvoie le type de retour d'une methode invoquee
 *)
let rec find_meth_type meth_name type_list class_called = 
	let class_name = Located.elem_of class_called in
	let class_loc = Located.loc_of class_called in
	match List.mem class_name protected_classes with
		| true -> non_existing_method class_name meth_name class_loc
		| _ 	 ->
			try
				let parent,atts,meths,loc = Hashtbl.find class_table class_name in
					try
						let sign,mloc = Hashtbl.find meths meth_name in
						let args = get_args_list sign in
						let res = get_return_type sign in
						check_args_type meth_name class_loc args type_list;
						res
					with Not_found ->
						match List.mem (Located.elem_of parent) protected_classes with
							| true 	-> non_existing_method class_name meth_name loc
							| _ 		-> find_meth_type meth_name type_list (Located.mk_elem (Located.elem_of parent) class_loc)
				with Not_found ->
					non_existing_class class_name class_loc

(**
 * Verifie que les attributs d'une classe n'ont pas deja ete 
 * declares dans une classe parent
 *)
let rec filter_atts att_list att_table = function
	| [] -> att_list
	| (name,atype,loc)::t ->
		begin
		match Hashtbl.mem att_table name with
			| true  -> used_att_name name loc
			| false -> filter_atts ((name,atype,loc)::att_list) att_table t
		end

(**
 * Verifie que les methodes d'une classe n'ont pas deja ete 
 * declares dans une classe parent ou qu'elles en respectent
 * la signature
 *)
let rec filter_meth_by_sig meth_list ploc meth_table = function
	| [] -> meth_list
	| (name,sign,mloc)::t ->
		try
			let (local_sign,loc) = Hashtbl.find meth_table name in
			match compare_sig sign local_sign with
				| 0	-> filter_meth_by_sig meth_list ploc meth_table t
				| _ -> sig_error name loc ploc
		with Not_found ->
			filter_meth_by_sig ((name,sign,mloc)::meth_list) ploc meth_table t

(**
 * Verifie :
 * Que le nom de classe n'est pas un nom protege
 * Qu'il n'herite pas d'une classe dont il serait parent (boucle d'heritage)
 * Que ces noms d'attributs et de methodes n'entrent pas en conflit avec ceux de ses parents 
 *)			
let rec check_inheritance att_list meth_list class_list key (parent,atts,meths,loc) =
	match List.mem key protected_classes with
		| true -> ()
		| _ 	 ->
			begin
			match List.mem key class_list with
				| true 	-> inheritance_loop key loc 
				| false ->
					let parent_name = Located.elem_of parent in
					match (stringOf parent_name) with
					| "Object" -> ()
					| _ 			 ->
						try
							let pparent,patts,pmeths,ploc = Hashtbl.find class_table parent_name in
							let remaining_atts = filter_atts [] patts att_list in
							let remaining_meths = filter_meth_by_sig [] ploc pmeths meth_list in
							check_inheritance remaining_atts remaining_meths (key::class_list) parent_name (pparent,patts,pmeths,ploc)
						with Not_found ->
							non_existing_class (Located.elem_of parent) (Located.loc_of parent)
			end

(* Construit la table des attributs d'une classe *)
let rec find_att att_table = function
		| [] 		-> att_table
		| h::t	-> 
			match Hashtbl.mem att_table h.aname with
				| true	-> used_att_name h.aname h.aloc
				| false ->
					Hashtbl.add att_table h.aname ((Located.elem_of h.atype),h.aloc);
					find_att att_table t

(* Construit la table des methodes d'une classe *)
let rec find_method meth_table = function
		| [] 		-> meth_table
		| h::t	->
			let name_used = Hashtbl.mem meth_table h.mname in
			begin
			match name_used with
				| true	-> used_meth_name h.mname h.mloc
				| false ->
					Hashtbl.add meth_table h.mname (create_sig h,h.mloc);
					find_method meth_table t
			end

(* Construit la table des classes d'un programme *)
let rec find_classes = function
		| [] 		-> ()
		| h::t	-> 
			match Hashtbl.mem class_table (fromString h.cname) with
				| true	-> used_class_name (fromString h.cname) h.cloc 
				| false ->
					begin
					match List.mem (fromString h.cname) protected_classes with
						| true	-> illegal_class_name (fromString h.cname) h.cloc
						| _			->
							begin
							match (stringOf (Located.elem_of h.cparent)) with 
								| "Int" | "String" | "Boolean" -> illegal_inheritance h.cname (Located.elem_of h.cparent) h.cloc
								| _ 													 ->
									let att_table = find_att (Hashtbl.create 0) h.cattributes in
									let meth_table = find_method (Hashtbl.create 0) h.cmethods in
									Hashtbl.add class_table (fromString h.cname) (h.cparent,att_table,meth_table,h.cloc);
									find_classes t
							end
					end

(* Retourne le type d'une operation *)
let call_type op first_typed typed_list =
	let first_type = get_type first_typed in
	let type_list = List.map (function hd -> get_type hd) typed_list in
	match List.mem first_type protected_classes with
	(* On traite d'abord les operations sur les Int, Boolean, et String *)
		| true -> 
			begin
			match op with
				| "not" 			->
					begin
					match (stringOf first_type) with
						| "Boolean" -> first_type
						| _					-> incorrect_type first_type (fromString "Boolean") first_typed.eloc
					end
				| "neg" 		->
					begin
					match (stringOf first_type) with
						| "Int" -> first_type
						| _			-> incorrect_type first_type (fromString "Int") first_typed.eloc
					end
				| "sub" | "add" | "mul" | "div" | "mod"	->
					let head_type = List.hd type_list in
					begin
					match (stringOf first_type),(stringOf head_type) with
						| "Int","Int" -> first_type
						| "Int",_ 		-> incorrect_type head_type (fromString "Int") (List.hd typed_list).eloc
						| _,"Int" 		-> incorrect_type first_type (fromString "Int") first_typed.eloc
						| _ 					-> incorrect_type first_type (fromString "Int") first_typed.eloc
					end	
				| "gt" | "ge" | "lt" | "le" ->
					let head_type = List.hd type_list in
					begin
					match (stringOf first_type),(stringOf head_type) with
						| "Int","Int" -> fromString "Boolean"
						| "Int",_ 		-> incorrect_type head_type (fromString "Boolean") (List.hd typed_list).eloc
						| _,"Int" 		-> incorrect_type first_type (fromString "Boolean") first_typed.eloc
						| _ 					-> incorrect_type first_type (fromString "Boolean") first_typed.eloc
					end
				| "eq" | "neq"	->
					let head_type = List.hd type_list in
					begin
					match compare_type first_type head_type with
						| true -> fromString "Boolean"
						| _ 	 -> incorrect_type first_type head_type first_typed.eloc
					end
				| "and" | "or"	->
					let head_type = List.hd type_list in
					begin
					match (stringOf first_type),(stringOf head_type) with
						| "Boolean","Boolean"	-> fromString "Boolean"
						| "Boolean",_ 				-> incorrect_type head_type (fromString "Boolean") (List.hd typed_list).eloc
						| _,"Boolean" 				-> incorrect_type first_type (fromString "Boolean") first_typed.eloc
						| _ 									-> incorrect_type first_type (fromString "Boolean") first_typed.eloc
					end
				| _ -> non_existing_method first_type op first_typed.eloc
			end
		(* Puis on traite les operations sur les autres objets *)
		| false ->
			begin
			match op with
				| "eq" | "neq"	->
					let head_type = List.hd type_list in
					begin
					match compare_type first_type head_type with
						| true -> fromString "Boolean"
						| _ 	 -> incorrect_type first_type head_type first_typed.eloc
					end
				(* Il 'sagit donc d'une invoquation de methode *)
				| _ as meth ->
					let class_type = Located.mk_elem first_type first_typed.eloc in
					find_meth_type op typed_list class_type
			end

(* retourne le type d'une expression *)
let rec type_expression class_name args_table local_table e =
	let curry_type_expression = type_expression class_name args_table local_table in
	match e.edesc with
		| New nval -> {edesc=New nval;etype=Some (Located.elem_of nval);eloc=e.eloc}
		| Seq (x,y)	->
			let expr_x = curry_type_expression x in
			let expr_y = curry_type_expression y in
			{edesc=Seq(expr_x,expr_y);etype=expr_y.etype;eloc=e.eloc}
		| Call (x,str,lst) ->
			let expr_x = curry_type_expression x in
			let typed_list = List.map curry_type_expression lst in
			let return_type = Some (call_type str expr_x typed_list) in
			{edesc=Call(expr_x,str,typed_list);etype=return_type;eloc=e.eloc}
		| If (x,y,z) -> 
			let expr_x  = curry_type_expression x in
			let cond 		= compare_type (get_type expr_x) (fromString "Boolean") in
			let expr_y 	= curry_type_expression y in 
			let expr_z 	= curry_type_expression z in
			let (res,par)	= has_common_parent (Located.mk_elem (get_type expr_y) y.eloc) (Located.mk_elem (get_type expr_z) z.eloc) in
				begin
				match cond,res,par with
				| (true,true,_)	-> {edesc=If(expr_x,expr_y,expr_z);etype=(Some par);eloc=e.eloc} 
				| (true,_,_) 		-> incorrect_type (get_type expr_y) (get_type expr_z) y.eloc
				| (_,_,_)				-> incorrect_type (get_type expr_x) (fromString "Boolean") x.eloc
				end
		| Val v 	-> {edesc=Val v;etype=Some (value_type v);eloc=e.eloc}
		|	Var str -> 
			begin
			match str with
				| "this" -> {edesc=Var str;etype=Some class_name;eloc=e.eloc}
				| _			 -> {edesc=Var str;etype=Some (find_type class_name args_table local_table e.eloc str);eloc=e.eloc}
			end
		| Assign (str,x) -> 
			let var_type = find_type class_name args_table local_table e.eloc str in
			let typed_exp = curry_type_expression x in
			begin
			match (var_type,typed_exp.etype) with
				| _ as t1, Some t2 ->
					begin
					match (is_parent (Located.mk_elem t1 typed_exp.eloc) (Located.mk_elem t2 typed_exp.eloc)) with
						| true -> {edesc=Assign(str,typed_exp);etype=Some t1;eloc=e.eloc}
						| _ 	 -> incorrect_var_type str t1 t2 e.eloc
					end
				| _ -> non_typed_exp typed_exp.eloc
			end
		| Define (str,val_type,x,y)	->
			let	var_type = Located.elem_of val_type in
			begin
			match exists_type var_type with
				| false	-> non_existing_class var_type (Located.loc_of val_type)
				| true	->
					let typed_x = curry_type_expression x in
					begin
					match typed_x.etype with
						| None 	 -> non_typed_exp typed_x.eloc
						| Some t -> 
							begin
							match (is_parent val_type (Located.mk_elem t typed_x.eloc)) with
								| true	->
									Hashtbl.add local_table str var_type;
									let typed_y = type_expression class_name args_table local_table y in
									Hashtbl.remove local_table str;
									{edesc=Define(str,val_type,typed_x,typed_y);etype=typed_y.etype;eloc=e.eloc}
								| _ 		-> incorrect_var_type str var_type t typed_x.eloc
							end	
					end
			end
		| Cast (new_type,x)	-> {edesc=Cast(new_type,curry_type_expression x);etype=Some (Located.elem_of new_type);eloc=e.eloc}
		| Instanceof (x,t)	-> {edesc=Instanceof(curry_type_expression x,t);etype=Some (fromString "Boolean");eloc=e.eloc}

(* Type les attributs d'une classe *)
let type_attribute class_name a = 
	let a_type = Located.elem_of a.atype in
	let correct_type = exists_type a_type in
	(* Verification de l'existence du type declare *)
	match correct_type,a.adefault with
		| false,_	-> non_existing_class a_type (Located.loc_of a.atype)
		|	_,None 		-> a
		|	_,Some e 	->
			(* Typage de l'expression d'initialisation *)
			let typed_exp = type_expression class_name (Hashtbl.create 0) (Hashtbl.create 0) e in		
			match typed_exp.etype with
				| None 		-> non_typed_exp typed_exp.eloc
				| Some t 	->
					let at = { 
						a with
						adefault = Some typed_exp;
					} in		
					begin
					(* Verification de la concordance entre type declare et type de l'expression *)
					match is_parent (Located.mk_elem a_type a.aloc) (Located.mk_elem t typed_exp.eloc) with
						| true 	-> at
						|	_ 		-> incorrect_var_type a.aname a_type t a.aloc
					end

(* Type les methodes d'une classe *)
let type_method class_name m =
	(* Creation et remplissage de la table des arguments de la methode *)
	let arg_table = Hashtbl.create (List.length m.margstype) in
	List.iter (fun(name,arg_type) -> Hashtbl.add arg_table name (Located.elem_of arg_type)) m.margstype;
	(* Typage du corps de la methode *)
	let typed_body = type_expression class_name arg_table (Hashtbl.create 0) m.mbody in
	let me = { 
		m with
			mbody = typed_body;
	} in
	match typed_body.etype with
		| None 		-> non_typed_exp typed_body.eloc 
		| Some t 	->
			begin
			(* Verification de la concordance entre type de retour declare et corps de la methode *)
			match is_parent m.mreturntype (Located.mk_elem t typed_body.eloc) with
				| true 	-> me 
				| _ 		-> return_type_error m.mname (Located.elem_of m.mreturntype) t  m.mloc
			end

(* Type une classe *)
let type_class c =
	(* Typage des attributs de la classe *)
	let typed_attr = List.map (type_attribute (fromString c.cname)) c.cattributes in
	(* Typage des methodes de la classe *)
	let typed_meth = List.map (type_method (fromString c.cname)) c.cmethods in
	let cl = { 
		c with
			cattributes = typed_attr;
			cmethods 		= typed_meth;
	} in
	cl

(* Transforme une Hashtbl en liste *)
let make_list h = 
	Hashtbl.fold (fun k (v,w) acc -> (k,v,w) :: acc) h []

(**
 * Type le programme sortant du Parser
 *
 * cl - liste d'astclass
 *
 * e_op - expressions optionnelles
 *)
let type_program (cl,e_op) = 
	Hashtbl.clear class_table;
	(* Remplissage de la Hashtable contenant les classes du programme mjava *)
	find_classes cl;
	(* Verification de l'héritage : non-bouclage, respect des attributs et des signatures des methodes*)
	Hashtbl.iter (fun key (parent,atts,meths,loc) -> 
		(check_inheritance (make_list atts) (make_list meths) [] key (parent,atts,meths,loc))) class_table;
	(* Typage de la table des classes *)
	let typed_cl = List.map type_class cl in
	(* Typage de l'expression optionnelle *)
	match e_op with
		| None 		-> typed_cl,None
		| Some e 	-> typed_cl,(Some (type_expression (fromString "") (Hashtbl.create 0) (Hashtbl.create 0) e))
