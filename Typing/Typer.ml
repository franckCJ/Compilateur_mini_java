open AST
open Type

let calc_bin_op = "sub"|"add"|"mul"|"div"|"mod"
let comp_bin_op = "gt"|"ge"|"lt"|"le"|"eq"|"neq"
let bool_bin_op = "and"|"or"

let compare_type t1 t2 =
	String.compare (stringOf t1) (stringOf t2)

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
		| _			-> (*Rechercher la méthode op dans la classe m=first_type*) fromString ""

let eval_type e = 
	match edesc with
		| New nval 								-> Some (elem_of nval)
		| Seq x y 								->
			type_expression x;
			type_expression y;
			Some ""
		| Call x str lst 					->
			let type_x = type_expression x in
			let type_list = List.map type_expression lst in
			Some (call_type str type_x type_list)
		| If x y z 								->
			let cond 		= String.compare (type_expression x).etype "Boolean" in
			let type_y 	= (type_expression y).etype in
			let type_z 	= (type_expression z).etype in
			let comp		= String.compare type_y type_z in
				match (cond,comp) with
					| (0,0) -> Some type_y
					| (0,_) -> print_endline "erreur de type"; Some ""
					| (_,0)	-> print_endline "erreur de type"; Some ""
		| Val v 									-> Some (value_type v)
		|	Var str 								-> (*Retrouver le type de la variable dans la table*) Some ""
		| Assign str x 						-> (*Retrouver le type de la var où on assigne*) type_expression x; Some "" (*A changer*)
		| Define str val_type x y	-> (*Ajouter une variable locale dans la table avec le type adéquat*) Some (elem_of val_type)
		| Cast new_type x 				-> type_expression x; Some (elem_of new_type)
		| Instanceof x t 					-> type_expression x; Some (fromString "Boolean")

let type_expression e =
	let exp = { 
		e with
		etype = eval_type e;
	} in
	exp

let type_attribute a = 
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

let verify_arg att_list arg =
	arg

let type_method att_list m =
	let method_args = List.map verify_arg att_list m.margstype in
	(*faire la liste des arguments de la méthode, vérifier qu'ils ne
	portent pas le meme nom qu'un attribut de la classe*)
	let typed_body = type_expression m.mbody in
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

let type_class c =
	let typed_attr = (List.map type_attribute c.cattributes) in
	let typed_meth = (List.map type_method typed_attr c.cmethods) in
	let cl = { 
		c with
			cattributes = typed_attr;
			cmethods 		= typed_meth;
	} in
	cl

let type_program (cl,e_op) = 
	let typed_cl = List.map type_class cl in
	match e_op with
		| None 		-> typed_cl,None
		| Some e 	-> typed_cl,(Some (type_expression e))
