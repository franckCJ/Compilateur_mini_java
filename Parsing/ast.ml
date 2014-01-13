open Location


(************ Partie Types AST *********)

(*type opérateur binaire*)
type binop =
	| Badd | Bsub
	| Bmul | Bdiv | Bmod
	| Binf | Binfeq | Bsup | Bsupeq
	| Bequal | Bdiff
	| Band | Bor
	| Bdel

(*type opérateur unaire*)
type unop =
	| Uopposite
	| Unot

(*type permettant de gérer les expressions dans l'arbre*)
type expression =
	| None
	| Self
	| Integer of int
	| String of string
	| Boolean of bool
	| Variable of string
	| Object of string
	| Binop of binop * expression * expression
	| Unop of unop * expression
	| Assignment of string * expression
	| Locassign of string * string * expression * expression
	| Condition of expression * expression * expression
	| Method_call of expression * string * expression list
	| Cast of expression * string
	| Instance of expression * string

(*type correspondant au paramètre d'une méthode*)
type param =
	| Param of string * string

(*Eléments internes de la classe : méthodes ou attributs*)
type class_intern =
	(*methode :     nom   +  type  +static?+ valeur*)
	| Attribute of string * string * bool * expression
	(*methode :  nom   +  type  +static?+liste params+  corps*)
	| Method of string * string * bool * param list * expression

(*Brique de bases d'un fichier, classes et expressions*)
type minijava =
	| Class of string * class_intern list * string
	| Expression of expression

(*type de base de l'arbre*)
type ast = minijava list

let rec display_list func elements =
	match elements with
		| []  	-> ""
		| [e]  	-> func e
		| t::q 	-> (func t) ^ "," ^ (display_list func q)

let string_of_binop = function
	| Badd 		-> "+"
	| Bsub 		-> "-"
	| Bmul 		-> "*"
	| Bdiv 		-> "/"
	| Bmod 		-> "%"
	| Binf 		-> "<"
	| Binfeq 	-> "<="
	| Bsup 		-> ">"
	| Bsupeq 	-> ">="
	| Bequal 	-> "=="
	| Bdiff 	-> "!="
	| Band 		-> "&&"
	| Bor 		-> "||"
	| Bdel 		-> ";"

let string_of_unop = function
	| Uopposite -> "-"
	| Unot 			-> "!"

let rec string_of_expr = function
	| None -> "None"
	| Self -> "Self"
	| Integer value -> string_of_int value
	| String value -> value
	| Boolean value -> string_of_bool value
	| Variable name -> name
	| Object name -> name
	| Binop (op,expr1,expr2) 	-> "(" ^ (string_of_expr expr1) ^ (string_of_binop op) ^ (string_of_expr expr2) ^ ")"
	| Unop (op,expr)  				-> (string_of_unop op) ^ (string_of_expr expr)
	| Assignment (name,expr) 	-> name ^ "=" ^ (string_of_expr expr)
	| Locassign (name,val_type,source,target) -> "(" ^ name ^","^ val_type ^ "," ^ (string_of_expr source) ^ ") -> " ^ (string_of_expr target)
	| Condition (exprif,exprthen,exprelse) -> "Condition(" ^ (string_of_expr exprif) ^ "," ^ (string_of_expr exprthen) ^ "," ^ (string_of_expr exprelse) ^ ")"
	| Method_call (caller,method_name,args) -> (string_of_expr caller) ^ "." ^ method_name ^ "(" ^ (display_list string_of_expr args) ^ ")"
	| Cast (expr,val_type) -> "(" ^ val_type ^ ")" ^ (string_of_expr expr)

let string_of_param = function
	| Param (name,par_type) -> "(" ^ name ^ "," ^ par_type ^ ")"

let string_of_func_interns = function
	| Attribute (name,att_type,stat,expr) 		-> "Attribute (" ^ name ^ "," ^ att_type ^ "," ^ (string_of_bool stat) ^ "," ^ (string_of_expr expr) ^ ")"
	| Method (name,ret_type,stat,params,expr)	-> "Method (" ^ name ^ "," ^ ret_type ^ "," ^ (string_of_bool stat) ^ "," ^ (display_list string_of_param params) ^ "," ^ (string_of_expr expr) ^ ")"

let string_of_minijava element =
	match element with
		| Class (name,interns,mother) -> "Class (" ^ name ^ "," ^ "[" ^ (display_list string_of_func_interns interns) ^ "], " ^ mother ^ ")"
		| Expression expr -> "Expr (" ^ (string_of_expr expr) ^ ")"

let string_of_file file = "File ( " ^ (display_list string_of_minijava file) ^ " )"

(************ Partie Erreurs *********)

(*erreurs survenant sur les chaines de caractère*)
type error_string =
	| Unvalid_escape
	| Open_string

(* Erreurs levées par le parser si besoin
	 Aucune des tentatives de gestion de l'erreur personnalisée liée au non respect des règles
	 de grammaire n'a été un succès. Gestion directement dans le fichier compile.ml
*)
type error =
	| Unexpected_syntax of char
	| Number_Exception of string
	| Unvalid_string of error_string
	| Open_comment
	| Syntax_error

(*Erreur envoyée contenant le type de l'erreur et se localisation*)
exception Compilation_Error of error * Location.t

(* Impression selon le type d'erreur levée *)
let report_error = function
	| Unexpected_syntax c ->
		print_string "Wrong character '";
		print_char c;
		print_string "'";
	| Number_Exception s ->
		print_string "The number ";
		print_string s;
		print_string " is too long";
	| Unvalid_string kind ->
		(match kind with
			| Unvalid_escape ->
				print_string "The character \\ is not escaping an other character"
			| Open_string ->
				print_string "The string is not closed")
	| Open_comment ->
		print_string "The comment is not closed"
	| Syntax_error ->
		print_string "Error in the syntax"
