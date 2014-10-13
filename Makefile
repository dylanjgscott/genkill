SRCDIR=src
VPATH=$(SRCDIR)
LG=alex
PG=happy

all: Lexer.hs Parser.hs

Lexer.hs: Lexer.x
	$(LG) $<

Parser.hs: Parser.y
	$(PG) $<

clean:
	rm $(SRCDIR)/Lexer.hs
	rm $(SRCDIR)/Parser.hs
