
all:
	ocamlbuild Main.byte

test: Test/Success/*.mjava
	./Main.byte $^

syntax: Test/Failure_Syntax/*.mjava
	./Main.byte $^

type: Test/Failure_Type/*.mjava
	./Main.byte $^

clean:
	ocamlbuild -clean
