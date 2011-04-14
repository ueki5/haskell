all:
	ghc -O2 --make TestParser.hs -prof -auto-all -caf-all -fforce-recomp -i../..
test1:
	ghc --make TestParser.hs -i../../
clean:
	rm TestParser
	rm TestParser.o
