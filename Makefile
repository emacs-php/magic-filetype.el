EMACS ?= emacs
CASK  ?= cask

all: cask test

cask:
	-cask

compile:
	${CASK} exec ${EMACS} -batch -Q -L . -eval \
	    "(progn \
	       (setq byte-compile-error-on-warn t) \
	       (batch-byte-compile))" \
	    *.el

test:
	${CASK} exec ert-runner

ci:
	${MAKE} clean
	${MAKE} test
	${MAKE} compile
	${MAKE} test
	${MAKE} clean

clean:
	-rm *.elc

clobber:
	rm -rf ./cask

.PHONY: all cask ci clean clobber compile test

# derived from http://www.kaichan.info/blog/2014-02-23-emacs-cask.html
