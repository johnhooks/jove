# -*- Makefile -*-

EMACS = /Applications/Emacs.app/Contents/MacOS/Emacs
# EMACS = emacs

# Compile with noninteractive and relatively clean environment.
BATCHFLAGS = -batch --quick

SRCS = jove-vars.el jove-lexer.el jove-parser.el jove-mode.el

OBJS = $(SRCS:.el=.elc)

%.elc: %.el
	${EMACS} $(BATCHFLAGS) -L . -f batch-byte-compile $^

all: $(OBJS)

clean:
	-rm -f $(OBJS)

test:
	${EMACS} $(BATCHFLAGS) -L . -l test/jove-lexer-test.el \
    -l test/jove-context-test.el -l test/jove-parser-test.el \
	-f ert-run-tests-batch-and-exit

