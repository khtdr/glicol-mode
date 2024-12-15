EMACS ?= emacs
BATCH := $(EMACS) -Q -batch -L .

.PHONY: all clean package install

all: package

clean:
	rm -rf *.elc

package:
	$(BATCH) -f batch-byte-compile *.el

install:
	mkdir -p ~/.emacs.d/lisp/glicol-mode
	cp *.el glicol.json ~/.emacs.d/lisp/glicol-mode/
