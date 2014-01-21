type obj_part = 
	|Null
	|True
	|False

type class =
	|Int of int
	|String of string
	|Boolean of boolean
	|Object
	|compiled_class

type compiled_class =
	{
		size : int;
		attr_list : attr list;
		meth_list : meth list;
	}

type attr =
	{
		aname: string;
		default_val : expression_desc;
	}

type meth =
	{
		mname: string;
	}

type meth_desc =
	{
		mdesc: string;
		mbody: expression_desc;
		margs: string list;
	}

let compile_class cl =
	match (Located.elem_of cl.cparent) with
		|"Object" ->
		|"Int" ->
		|"String" ->
		|"Boolean" ->

let compile (cl,e_op) = 
	let compiled_classes = List.map compile_class cl in