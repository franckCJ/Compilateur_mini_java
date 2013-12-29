type minijava =
	| File of minijava list
	| Class of string * minijava list
	| Attribute of string
	| Method of string * minijava list
