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

optimizer: Optimizer.hs
	$(HC) --make -i -o $@ $< 

tester: Tester.hs optimizer CFG.hs CFGTests.hs TestLib.hs
	$(HC) --make -i$(SRCDIR):$(TEST_SRC) -o $@ $<
    
test: tester
	./tester

                    
clean:
	rm -f $(SRCDIR)/Lexer.hs
	rm -f $(SRCDIR)/Parser.hs
	rm -f optimizer
	rm -f $(SRCDIR)/Optimizer.hi
	rm -f $(SRCDIR)/Optimizer.o
	rm -f $(SRCDIR)/CFG.hi
	rm -f $(SRCDIR)/CFG.o
	rm -f tester
	rm -f $(TEST_SRC)/Tester.hi
	rm -f $(TEST_SRC)/Tester.o
	rm -f $(TEST_SRC)/TestLib.hi
	rm -f $(TEST_SRC)/TestLib.o
	rm -f $(TEST_SRC)/CFGTests.hi
	rm -f $(TEST_SRC)/CFGTests.o


