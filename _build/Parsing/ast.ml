open Location

type binop =
	| Badd | Bsub
	| Bmul | Bdiv | Bmod
	| Binf | Binfeq | Bsup | Bsupeq
	| Bequal | Bdiff
	| Band | Bor
	| Bdel

type unop =
	| Uopposite
	| Unot

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

type param =
	| Param of string * string

type minijava =
	| File of minijava list
	| Class of string * minijava list
	(*methode : nom+type+static?+valeur*)
	| Attribute of string * string * bool * expression
	(*methode : nom+type+static?+liste param+corps*)
	| Method of string * string * bool * param list * expression
	| Expression of expression

type error_string =
	| Unvalid_escape
	| Open_string

type error =
	| Unexpected_syntax of char
	| Number_Exception of string
	| Unvalid_string of error_string
	| Open_comment
	| Syntax_error

exception Compilation_Error of error * Location.t

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
				print_string "The character \ is not escaping an other character"
			| Open_string ->
				print_string "The string is not closed")
	| Open_comment ->
		print_string "The comment is not closed"
	| Syntax_error ->
		print_string "Error in the syntax"
