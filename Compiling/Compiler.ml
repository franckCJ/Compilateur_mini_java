open AST
open Env

type value = 
  | String of string
  | Int of int
  | Null
  | Boolean of bool

type expression_desc = 
  | New of Type.t Located.t
  | Seq of expression * expression
  | Call of expression * string * expression list
  | If of expression * expression * expression
  | Val of value
  | Var of string
  | Assign of string * expression
  | Define of string * Type.t Located.t * expression * expression
  | Cast of Type.t Located.t * expression
  | Instanceof of expression * Type.t Located.t

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

let rec compile_class cl =
	let mother_class = Located.elem_of cl.cparent in
	if (mem class_desc cl)
	
	match (Located.elem_of cl.cparent) with
		|"Object" ->
		|"Int" ->
		|"String" ->
		|"Boolean" ->

let compile (cl,e_op) = 
	let class_desc = in
	let object_desc = in
	let meth_table = in
	let compilation_tables = List.map compile_class cl in
	compilation_tables,op

