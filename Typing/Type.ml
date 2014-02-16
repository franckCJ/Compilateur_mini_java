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

let get_args_list (args,res) =
	args

let get_return_type (args,res) =
	res
