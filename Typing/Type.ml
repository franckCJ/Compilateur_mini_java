type t = string

type func = t list * t

let stringOf t = t

let fromString t = t

let create_func t = t

let rec string_of_list = function
  | [] -> ""
  | [t] -> t
  | t::l -> t^"*"^(string_of_list l)

let string_of_func (args,res) =
  "("^(string_of_list args)^") -> "^res

let type_expression e =

let type_class c =
	List.iter type_attribute
	List.iter type_method

let type_program (cl,e_op) = 
	let typed_cl = List.iter type_class cl in
	match e_op with
		| None 		-> typed_cl,None
		| Some e 	-> typed_cl,(type_expression e)
