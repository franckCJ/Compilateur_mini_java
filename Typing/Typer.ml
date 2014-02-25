open AST
open Type
open TypeError;;

let protected_classes = [fromString "Object" ; fromString "Int" ; fromString "String" ; fromString "Boolean"]

let class_table = Hashtbl.create 0

let equals_string s1 s2 =
	let comp = String.compare s1 s2 in
	match comp with
		| 0 -> true
		| _ -> false

let compare_type t1 t2 =
	match String.compare (stringOf t1) (stringOf t2) with
		| 0 -> true
		| _ -> false 

let compare_sig s1 s2 =
	String.compare (string_of_func s1) (string_of_func s2)

let get_type = function
	| None		-> non_typed_exp
	| Some t	-> t

let exists_type value_type = 
	match List.mem value_type protected_classes with
		| true -> true
		| _ 	 -> Hashtbl.mem class_table value_type

let rec is_parent t1 t2 =
	let type_t2 = Located.elem_of t2 in
	try
		let parent,atts,meths,loc = Hashtbl.find class_table type_t2 in
		let type_parent = Located.elem_of parent in
		match List.mem type_parent protected_classes with 
			|	true -> false
			| _ 	->
				let type_t1 = Located.elem_of t1 in
				begin
				match type_parent with
					| type_t1	-> true
					| _  			-> is_parent t1 parent
				end
	with Not_found ->
		non_existing_class (Located.elem_of t2) (Located.loc_of t2)

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
		
let find_type class_name args_table local_table var_loc var_name =
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
				(*Voir si pas prendre loc en entrée de la fonction pour avoir une erreur plus précise*)
				non_existing_attribute class_name var_name var_loc

let rec check_args_type meth_name loc args = function
	| [] ->
		begin
		match args with
			| [] 	-> ()
			| _		-> args_error meth_name -1 loc
		end
	| h::t ->
		begin
		match args with
			| []			-> args_error meth_name 1 h.eloc
			| ah::at	->
				let ref_arg = Located.mk_elem ah h.eloc in
				let used_arg = Located.mk_elem (get_type h.etype) h.eloc in
				begin
				match is_parent ref_arg used_arg with
					| true	-> check_args_type meth_name h.eloc at t
					| false	-> args_error meth_name 0 h.eloc
				end
		end

let rec find_meth_type meth_name type_list class_called = 
	let class_name = Located.elem_of class_called in
	let class_loc = Located.loc_of class_called in
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
					| true 	-> non_existing_method (stringOf class_name) meth_name loc
					| _ 		-> find_meth_type meth_name type_list (Located.mk_elem (Located.elem_of parent) class_loc)
		with Not_found ->
			non_existing_class class_name class_loc

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
				| _ -> sig_error name loc exp_loc
		with Not_found ->
			filter_meth_by_sig ((name,sign)::meth_list) meth_table t
			
let rec check_extends_cycle meth_list class_list key (parent,atts,meths,loc) =
	match List.mem key protected_classes with
		| true -> ()
		| _ 	 ->
			begin
			match List.mem key class_list with
				| true 	-> inheritance_loop (stringOf key) loc 
				| false ->
					let parent_name = Located.elem_of parent in
					try
						let pparent,patts,pmeths,ploc = Hashtbl.find class_table parent_name in
						let remaining_meths = filter_meth_by_sig [] pmeths meth_list in
						check_extends_cycle remaining_meths (key::class_list) parent_name (pparent,patts,pmeths,ploc)
					with Not_found ->
						non_existing_class (stringOf (Located.elem_of parent)) (Located.loc_of parent)
			end

let rec find_att att_table att_list =
	match att_list with
		| [] 		-> att_table
		| h::t	-> 
			let name_used = Hashtbl.mem att_table h.aname in
			match name_used with
				| true	-> used_att_name h.aname h.aloc
				| false ->
					Hashtbl.add att_table h.aname (Located.elem_of h.atype,h.aloc);
					find_att att_table t

let create_sig m =
	let name_list,type_list = List.split m.margstype in
	let args_type = List.map Located.elem_of type_list in
	let return_type = Located.elem_of m.mreturntype in
	create_func (args_type,return_type)

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

(* Vérifie si le nom de la classe ne correspond pas à Int, String, Boolean, Null,
	Object => classes protégées *)
let rec find_classes = function
		| [] 		-> ()
		| h::t	-> 
			let name_used = Hashtbl.mem class_table (fromString h.cname) in
			match name_used with
				| true	-> used_class_name h.cname h.cloc 
				| false ->
					begin
					match List.mem (fromString h.cname) protected_classes with
						| true	-> illegal_class_name h.cname h.cloc
						| _			->
							let att_table = find_att (Hashtbl.create 0) h.cattributes in
							let meth_table = find_method (Hashtbl.create 0) h.cmethods in
							Hashtbl.add class_table (fromString h.cname) (h.cparent,att_table,meth_table,h.cloc);
							find_classes t
					end


(*********** Détermination des correspondances des types     **********)
let value_type = function
	| String s 	-> fromString "String"
	| Int i 		-> fromString "Int"
	| Null 			-> fromString "Null"
	| Boolean b -> fromString "Boolean"

let call_type op first_typed typed_list =
	let first_type = get_type (first_typed.etype) in
	let type_list = List.map (function hd -> get_type hd.etype) typed_list in
	let head_type = List.hd type_list in
	match op with
		| "not" 			->
			begin
			match (stringOf first_type) with
				| "Boolean" -> first_type
				| _					-> incorrect_type first_type first_typed.eloc (fromString "Boolean") Location.none
			end
		| "neg" 			->
			begin
			match (stringOf first_type) with
				| "Int" -> first_type
				| _			-> incorrect_type fist_type first_typed.eloc (fromString "Int") Location.none
			end
		| "sub" | "add" | "mul" | "div" | "mod"	->
			begin
			match (stringOf first_type),(stringOf head_type) with
				| "Int","Int" -> first_type
				| "Int",_ 		-> incorrect_type head_type (List.hd typed_list).eloc (fromString "Int") Location.none
				| _,"Int" 		-> incorrect_type first_type first_typed.eloc (fromString "Int") Location.none
				| _ 					-> incorrect_type first_type first_typed.eloc (fromString "Int") Location.none
			end	
		| "gt" | "ge" | "lt" | "le" | "eq" | "neq"	->
			begin
			match (stringOf first_type),(stringOf head_type) with
				| "Int","Int" -> fromString "Boolean"
				| "Int",_ 		-> incorrect_type head_type (List.hd typed_list).eloc (fromString "Boolean") Location.none
				| _,"Int" 		-> incorrect_type first_type first_typed.eloc (fromString "Boolean") Location.none
				| _ 					-> incorrect_type first_type first_typed.eloc (fromString "Boolean") Location.none
			end
		| "and" | "or"	->
			begin
			match (stringOf first_type),(stringOf head_type) with
				| "Boolean","Boolean"	-> fromString "Boolean"
				| "Boolean",_ 				-> incorrect_type head_type (List.hd typed_list).eloc (fromString "Boolean") Location.none
				| _,"Boolean" 				-> incorrect_type first_type first_typed.eloc (fromString "Boolean") Location.none
				| _ 									-> incorrect_type first_type first_typed.eloc (fromString "Boolean") Location.none
			end
		| _ as meth		->
			let class_type = Located.mk_elem first_type first_typed.eloc in
			find_meth_type meth typed_list class_type

let rec type_expression class_name args_table local_table e =
	let curry_type_expression = type_expression class_name args_table local_table in
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
					Some (call_type str typed_x typed_list)
				| If (x,y,z) 								->
					let type_x  = get_type (curry_type_expression x).etype in
					let cond 		= compare_type type_x (fromString "Boolean") in
					let type_y 	= get_type (curry_type_expression y).etype in 
					let type_z 	= get_type (curry_type_expression z).etype in
					let result	= has_common_parent (Located.mk_elem type_y y.eloc) (Located.mk_elem type_z z.eloc) in
						begin
						match cond,result with
							| (0,(true,_ as t))	-> Some t 
							| (0,_) 						-> incorrect_type type_y y.eloc type_z z.eloc
							| (_,_)							-> incorrect_type type_x x.eloc (fromString "Boolean") Location.none
						end
				| Val v 	-> Some (value_type v)
				|	Var str -> 
					begin
					match str with
						| "this"	-> Some class_name
						| _	as s	-> Some (find_type class_name args_table local_table e.eloc s)
					end
				| Assign (str,x) -> 
					let var_type = find_type class_name args_table local_table e.eloc str in
					let typed_exp = curry_type_expression x in
					begin
					match (var_type,typed_exp.etype) with
						| t1, Some t2 ->
							begin
							match (is_parent t1 t2) with
								| true 	-> Some t1
								| _ 		-> incorrect_var_type str t1 t2 e.eloc
							end
						| _ -> non_typed_exp typed_exp.eloc
					end
				| Define (str,val_type,x,y)	->
					let	var_type = Located.elem_of val_type in
					begin
					match exists_type var_type with
						| false	-> non_existing_class var_type
						| true	->
							let typed_x = curry_type_expression x in
							begin
							match typed_x.etype with
								| None -> non_typed_exp typed_x.eloc
								| Some t -> 
									begin
									match (is_parent var_type t) with
										| true	->
											Hashtbl.add local_table str var_type;
											let typed_y = type_expression class_name args_table local_table y in
											Hashtbl.remove local_table str;
											typed_y.etype
										| _ 		-> incorrect_var_type str var_type t
									end	
							end
					end
				| Cast (new_type,x)				-> curry_type_expression x; Some (Located.elem_of new_type)
				| Instanceof (x,t)				-> curry_type_expression x; Some (fromString "Boolean")
	} in
	exp

let type_attribute class_name a = 
	let a_type = Located.elem_of atype in
	let correct_type = exists_type a_type in
	match correct_type,a.adefault with
		| false,_	-> non_existing_class a_type (Located.loc_of a.atype)
		|	_,None 		-> a
		|	_,Some e 	->
			let typed_exp = type_expression class_name (Hashtbl.create 0) (Hashtbl.create 0) e in
			let at = { 
				a with
				adefault = Some typed_exp;
			} in				
			match typed_exp.etype with
				| None 		-> non_typed_exp typed_exp.etype
				| Some t 	->
					begin
					match is_parent a_type t with
						| true 	-> at
						|	_ 		-> incorrect_var_type a.aname a_type t a.aloc
					end

(*verifier les types d'entrée et de retour existant*)
let type_method class_name m =
	let arg_table = Hashtbl.create (List.length m.margstype) in
	List.iter (fun(name,arg_type) -> Hashtbl.add arg_table name (Located.elem_of arg_type)) m.margstype;
	let typed_body = type_expression class_name arg_table (Hashtbl.create 0) m.mbody in
	let me = { 
		m with
			mbody = typed_body;
	} in
	match typed_body.etype with
		| None 		-> non_typed_exp typed_body.eloc 
		| Some t 	->
			begin
			match is_parent (Located.elem_of m.mreturntype) t with
				| true 	-> me 
				| _ 		-> return_type_error m.mname (Located.elem_of m.mreturntype) t  m.mloc
			end

let type_class c =
	let typed_attr = List.map (type_attribute (fromString c.cname)) c.cattributes in
	let typed_meth = List.map (type_method (fromString c.cname)) c.cmethods in
	let cl = { 
		c with
			cattributes = typed_attr;
			cmethods 		= typed_meth;
	} in
	cl

let get_keys htbl =
	let result = [] in
	Hashtbl.iter (fun k v -> k::result;()) htbl;
	result

let type_program (cl,e_op) = 
	find_classes cl;
	Hashtbl.iter (fun key (parent,atts,meths,loc) -> (check_extends_cycle (get_keys meths) [] key (parent,atts,meths,loc))) class_table;
	let typed_cl = List.map type_class cl in
	match e_op with
		| None 		-> typed_cl,None
		| Some e 	-> typed_cl,(Some (type_expression (fromString "") (Hashtbl.create 0) (Hashtbl.create 0) e))
