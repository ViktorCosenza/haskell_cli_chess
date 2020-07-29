GHC="ghc"
OUT="chessgame"
all:
	$(GHC) -o $(OUT) Main.hs

clean:
	rm -f *.hi *.o $(OUT)