all: Main

Lex.hs : Lex.x
	alex Lex.x

Parse.hs : Parse.y
	happy Parse.y
    
Main : Lex.hs Parse.hs Main.hs
	ghc --make Main
    
clean:
	rm -f *.o *.hi Parse.hs Lex.hs
