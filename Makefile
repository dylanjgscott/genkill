SRCDIR=src
TEST_SRC=tests
VPATH=$(SRCDIR):$(TEST_SRC)
LG=alex
PG=happy
HC=ghc

all: Lexer.hs Parser.hs optimizer

Lexer.hs: Lexer.x Token.hs
	$(LG) $<

Parser.hs: Parser.y Token.hs
	$(PG) $<

optimizer: Optimizer.hs Cfg.hs Deadcode.hs Unreachable.hs RedReg.hs Parser.hs Lexer.hs Genkill.hs Assembly.hs Util.hs
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
	rm -f $(SRCDIR)/RedReg.hi
	rm -f $(SRCDIR)/RedReg.o
	rm -f $(SRCDIR)/Lexer.hi
	rm -f $(SRCDIR)/Lexer.o
	rm -f $(SRCDIR)/Parser.hi
	rm -f $(SRCDIR)/Parser.o
	rm -f $(SRCDIR)/Genkill.hi
	rm -f $(SRCDIR)/Genkill.o
	rm -f $(SRCDIR)/Assembly.hi
	rm -f $(SRCDIR)/Assembly.o
	rm -f $(SRCDIR)/Util.hi
	rm -f $(SRCDIR)/Util.o
	rm -f tester
	rm -f $(TEST_SRC)/Tester.hi
	rm -f $(TEST_SRC)/Tester.o
	rm -f $(TEST_SRC)/TestLib.hi
	rm -f $(TEST_SRC)/TestLib.o
	rm -f $(TEST_SRC)/CFGTests.hi
	rm -f $(TEST_SRC)/CFGTests.o
