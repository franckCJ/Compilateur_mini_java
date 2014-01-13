open Location

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
	(*methode : nom+type+static?+valeur*)
	| Attribute of string * string * bool * expression
	(*methode : nom+type+static?+liste param+corps*)
	| Method of string * string * bool * param list * expression

(*types de base de l'arbre*)
type minijava =
	| File of minijava list
	| Class of string * class_intern list
	| Expression of expression

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
