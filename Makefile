SRC=$(shell find . -type f -name "ex*.ml" | sort)

.PHONY: all
all: build

.PHONY: build
build: a.out

.PHONY: test
test: build
	./a.out

metro.ml:
	curl -s http://pllab.is.ocha.ac.jp/~asai/book-data/metro.ml \
		| iconv -f EUC-JP -t UTF-8 > metro.ml

a.out: metro.ml
	ocamlc -o a.out metro.ml ${SRC}

.PHONY: clean
clean:
	rm -rf a.out
	find . -type f -name "*.cm[aiox]" | xargs rm -rf
