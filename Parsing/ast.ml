type minijava =
	| File of minijava list
	| Class of string * minijava list
	| Attribute of string
	| Method of string * minijava list

type error_string =
	| Unvalid_escape
	| Open_string

type error =
	| Illegal_character of char
	| Number_Exception of string
	| Unvalid_string of error_string

exception Error of error * Location.t

let rec count_line curr_string lexbuf =
	parse curr_string with
		| '\n' -> Location.incr_line lexbuf
		| _ -> count_line curr_string lexbuf

(*let rec read_string current_string lexbuf =
	match lexbuf with
	| "\\\\" -> read_string (current_string ^ "\\") lexbuf
	| "\n" -> read_string current_string lexbuf
	| "\"" -> read_string (current_string ^ "\"") lexbuf
	| "\"" -> current_string
	| "\\" -> raise (Error (Unvalid_string Unvalid_escape, (Location.curr lexbuf)))
	| eof -> raise (Error (Unvalid_string Open_string, (Location.curr lexbuf)))
	| _ as c -> read_string (current_string ^ c) lexbuf
	*)
