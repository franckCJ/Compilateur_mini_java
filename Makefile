
all:
	ocamlbuild Main.byte

test: Test/Success/*.mjava Test/Failure_Type/*.mjava Test/Failure_Syntax/*.mjava 
	./Main.byte $^

type: Test/Failure_Type/*.mjava
	./Main.byte $^

clean:
	ocamlbuild -clean
