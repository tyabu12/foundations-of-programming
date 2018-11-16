.PHONY: all
all: test

.PHONY: test
test: metro.ml
	./run_tests.sh

metro.ml:
	curl -s http://pllab.is.ocha.ac.jp/~asai/book-data/metro.ml | iconv -f EUC-JP -t UTF-8 > metro.ml

.PHONY: clean
clean:
	rm -rf a.out
	find . -type f -name "*.cm[aiox]" | xargs rm -rf
