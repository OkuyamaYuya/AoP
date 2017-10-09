all: Main

LEX = Lex.x
PARSE = Parse.y
MAIN = Main.hs

Lex.hs : $(LEX)
	alex $(LEX)

Parse.hs : $(PARSE)
	happy $(PARSE)

Main : Lex.hs Parse.hs $(MAIN)
	stack ghc -- --make $(MAIN) -o gd

clean:
	rm -f *.o *.hi Parse.hs Lex.hs
