
all:
	ocamlbuild Main.byte

test: Test/*.mjava
	./Main.byte $^

clean:
	ocamlbuild -clean
