
all:
	ghc --make Graphes.hs
	make clean
clean:
	rm *.o
	rm *.hi
