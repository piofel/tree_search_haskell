liat_sw_1: SearchProblemDefinition.o TreeSearch.o Main.o
	ghc -o liat_sw_1 SearchProblemDefinition.o TreeSearch.o Main.o 

clean:
	rm -f *.hi *.o

SearchProblemDefinition.o: SearchProblemDefinition.hs
	ghc -c SearchProblemDefinition.hs

TreeSearch.o: TreeSearch.hs
	ghc -c TreeSearch.hs

Main.o: Main.hs
	ghc -c Main.hs

