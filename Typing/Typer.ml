open AST
open Type

let compare_type t1 t2 =
	String.compare (stringOf t1) (stringOf t2)

let rec eval_type e =
	match e.edesc with
		|New type -> type
		|Seq x y -> 
		|Call x str lst ->
		|If x y z -> let type_y = (eval_type y) in
					 let bool = String.compare (eval_type x) "boolean" in
					 let comp = String.compare type_y (eval_type z) in
					 match (bool,comp) with
					 	|(0,0) -> type_y
					 	|_ -> print_endline "erreur de type"; ""
		|Val x -> match x with
					|Int i -> "int"
					|String str -> "string"
  					|Null -> "null"
  					|Boolean bool -> "boolean"
		|Var str ->
		|Assign str x -> eval_type x
		|Define str type x y -> type
		|Cast type x -> type
		|Instanceof x type -> type

let type_expression e =
	let exp = { 
		e with
		etype = eval_type e;
	} in
	exp

let type_attribute a = 
	match a.adefault with
		|None -> a
		|Some e -> let typed_exp = type_expression e in
					let at = { 
						a with
						adefault = Some typed_exp;
					} in				
				match typed_exp.etype with
					|None -> print_endline "erreur de type"; a
					|Some t -> let comp = compare_type t (Located.elem_of a.atype) in
						match comp with
							|0 -> at
							|_ -> print_endline "erreur de type"; a

let type_method m =
	let typed_body = type_expression m.mbody in
	let me = { 
		m with
			mbody = typed_body;
	} in
	match typed_body.etype with
		|None -> print_endline "erreur de type"; m
		|Some t -> let comp = compare_type t (Located.elem_of m.mreturntype) in
				match comp with
					|0 -> me 
					|_ -> print_endline "erreur de type"; m

let type_class c =
	let typed_attr = (List.map type_attribute c.cattributes) in
	let typed_meth = (List.map type_method c.cmethods) in
	let cl = { 
		c with
			cattributes = typed_attr;
			cmethods = typed_meth;
	} in
	cl


let type_program (cl,e_op) = 
	let typed_cl = List.map type_class cl in
	match e_op with
		| None 		-> typed_cl,None
		| Some e 	-> typed_cl,(Some (type_expression e))