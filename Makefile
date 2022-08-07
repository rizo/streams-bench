
./standalone: src/Standalone.ml
	ocamlfind opt \
		-O3 \
		-package benchmark \
		-o ./standalone -linkpkg src/Standalone.ml

