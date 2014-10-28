SRCDIR=src
TEST_SRC=tests
VPATH=$(SRCDIR):$(TEST_SRC)
LG=alex
PG=happy
HC=ghc

all: Lexer.hs Parser.hs optimizer

Lexer.hs: Lexer.x
	$(LG) $<

Parser.hs: Parser.y
	$(PG) $<

optimizer: Optimizer.hs Cfg.hs Deadcode.hs Unreachable.hs Parser.hs Lexer.hs
	$(HC) --make -i$(SRCDIR) -o $@ $< 

tester: Tester.hs optimizer Cfg.hs CFGTests.hs TestLib.hs
	$(HC) --make -i$(SRCDIR):$(TEST_SRC) -o $@ $<
    
test: tester
	./tester
                    
clean:
	rm -f $(SRCDIR)/Lexer.hs
	rm -f $(SRCDIR)/Parser.hs
	rm -f optimizer
	rm -f $(SRCDIR)/Optimizer.hi
	rm -f $(SRCDIR)/Optimizer.o
	rm -f $(SRCDIR)/Cfg.hi
	rm -f $(SRCDIR)/Cfg.o
	rm -f $(SRCDIR)/Unreachable.hi
	rm -f $(SRCDIR)/Unreachable.o
	rm -f $(SRCDIR)/Deadcode.hi
	rm -f $(SRCDIR)/Deadcode.o
	rm -f tester
	rm -f $(TEST_SRC)/Tester.hi
	rm -f $(TEST_SRC)/Tester.o
	rm -f $(TEST_SRC)/TestLib.hi
	rm -f $(TEST_SRC)/TestLib.o
	rm -f $(TEST_SRC)/CFGTests.hi
	rm -f $(TEST_SRC)/CFGTests.o
