SRC = src/metro.ml src/gakusei.ml src/checkRaise.ml src/redBlack.ml
SRC += $(shell find ./src -type f -name "ex*.ml" | sort)

.PHONY: all
all: build

.PHONY: build
build: a.out

.PHONY: test
test: build
	./a.out

src/metro.ml:
	curl -s http://pllab.is.ocha.ac.jp/~asai/book-data/metro.ml \
		| iconv -f EUC-JP -t UTF-8 > src/metro.ml

src/redBlack.cmi:
	ocamlc -c src/redBlack.mli

a.out: ${SRC} src/redBlack.cmi
	ocamlc -o a.out -I ./src ${SRC}

.PHONY: clean
clean:
	rm -rf a.out
	find ./src -type f -name "*.cm[aiox]" | xargs rm -rf
