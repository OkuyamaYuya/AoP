all: Main

LEX = Lex.x
PARSE = Parse.y
MAIN = Main.hs

Lex.hs : $(LEX)
	alex $(LEX)

Parse.hs : $(PARSE)
	happy $(PARSE)

Main : Lex.hs Parse.hs $(MAIN)
	ghc --make $(MAIN)

clean:
	rm -f *.o *.hi Parse.hs Lex.hs
