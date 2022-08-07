
./standalone: src/Standalone.ml
	ocamlfind opt -O3 -unbox-closures -package benchmark \
		-o ./standalone -linkpkg src/Standalone.ml

