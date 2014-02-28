open AST
open Env

(**
 * Trouve une classe selon son nom dans un liste
 * 
 * Prend la liste des classes
 * Prend le nom recherché
 *
 * Renvoie le couple classe_trouvé,reste_liste
 *)

let rec find_class name class_list =
	match class_list with
		| [e] 	-> e,[]
		| h::t	-> 
			begin
			match String.compare h.cname name with
				| 0	-> h,t
				| _	->
					let found,remain = find_class name t in
					found,h::remain
			end

(**
 * Compile la liste des attributs pour le descripteur d'objet
 * 
 * Prend la liste des attributs d'une classe
 *
 * Renvoie la liste des attributs contenant le nom la valeur par
 * défaut
 *)

let rec compile_att att_list =
	match att_list with
		| []		-> []
		| h::t	->(h.aname,h.adefault)::compile_att t

(**
 * Supprime les méthodes de même nom de la liste
 * 
 * Prend la liste des méthodes
 * Prend le nom qui autorise la suppression
 *
 * Renvoie la liste actualisée
 *)

let rec remove_meth meth_name = function
	| []		-> []
	|	h::t	->
		let beg_index = 
		try
			 (String.index h '-')+1
		with Not_found	-> 0
		in 
		let end_index = (String.length h) in
		let sub_string = String.sub h beg_index (end_index-beg_index) in
		begin
		match String.compare sub_string meth_name with
			| 0	-> t
			| _	-> h::(remove_meth meth_name t)
		end

(**
 * Compile la liste des méthodes
 *
 * Prend la classe qui possède les méthode 
 * Prend les méthodes compilées de la classe mère
 * Prend la table à mettre à jour
 *
 * Renvoie la table des méthodes complétée ainsi que la liste des
 * noms des méthodes de la classe
 *)

let rec compile_meth cl meth_table mother_meth meth_list =
	match meth_list with
		| [] 		-> meth_table,mother_meth
		| h::t	->
			let new_moth_meth = remove_meth h.mname mother_meth in
			let curr_name = (cl ^ "-" ^ h.mname) in
			let args,loc = List.split h.margstype in
			let new_meth_table = define meth_table curr_name (h.mbody,args) in
			let (new_meth_table,new_list) = compile_meth cl new_meth_table new_moth_meth t in
			new_meth_table,(curr_name::new_list)

(**
 * Compile la classe passée en paramètre et complète les tables 
 * Peut s'appeler récursivement sur la classe mère si celle-ci n'est pas compilée
 *
 * Prend la classe et les tables en entrée
 *
 * Renvoie les tables mises à jour
 *)

let rec compile_class meth_table class_desc_table object_desc_table clist cl =
	let mother_type = Located.elem_of cl.cparent in
	(* Récupération des tables courantes possiblement mises à jour par la compilation de la classe mère *)
	let curr_meth_table,curr_class_desc_table,curr_object_desc_table = 
	match mem class_desc_table mother_type with
		| false	->
			let (mother_class,remain_list) = find_class (Type.stringOf mother_type) clist in
			compile_class meth_table class_desc_table object_desc_table remain_list mother_class
		| true	-> meth_table,class_desc_table,object_desc_table
	in
	(* Création des descripteurs *)
	let parent::mother_meth = find curr_class_desc_table mother_type in
 	let (new_meth_table,meth_name_list) = compile_meth cl.cname curr_meth_table mother_meth cl.cmethods in
	let class_desc = ((Type.stringOf mother_type)::meth_name_list) in
	let mother_object = find curr_object_desc_table mother_type in
	let object_desc = mother_object@(compile_att cl.cattributes) in
	(* Remplissage des tables *)
	let new_class_desc_table = define class_desc_table (Type.fromString cl.cname) class_desc in
	let new_object_desc_table = define object_desc_table (Type.fromString cl.cname) object_desc in
	new_meth_table,new_class_desc_table,new_object_desc_table

(**
 * Applique la compilation sur chacune des classes de la liste en mettant 
 * les tables a jour
 *
 * Prend en entrée les tables de base et la liste des classes 
 *
 * Renvoie les tables
 *)

let rec class_compilation clist meth_table class_desc object_desc = function
	| [] 		-> meth_table,class_desc,object_desc
	| h::t	->
		let nmtable,ncdesc,nobjdesc =
			compile_class meth_table class_desc object_desc clist h
		in
		class_compilation clist nmtable ncdesc nobjdesc t

(**
 * Compile le programme sortant du typage
 * 
 * cl -> listes des classes typees
 * e_op	-> expression principale typee
 * 
 * Renvoie les tables globales des methodes, des descripteurs de classe, et
 * des descripteurs d'objet, ainsi que l'expression de plus haut niveau
 *)

let compile_program (cl,e_op) = 
	let class_desc = initial () in
	let object_desc = initial () in
	let meth_table = initial () in
	let initialised_class_desc = define class_desc (Type.fromString "Object") ["Object"] in
	let initialised_object_desc = define object_desc (Type.fromString "Object") [] in
	let compile_func = class_compilation cl meth_table initialised_class_desc initialised_object_desc in
	let compilation_tables = compile_func cl in
	compilation_tables,e_op

