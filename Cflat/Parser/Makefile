TestParser:TestParser.hs Parser.hs
	ghc  --make TestParser.hs -fforce-recomp -i../../
TestParserP:TestParser.hs Parser.hs
	ghc  -O2 --make TestParser.hs -prof -auto-all -caf-all -fforce-recomp -i../../
run:TestParser
	./TestParser
perf:TestParserP
	time ./TestParser +RTS -p -K100M -RTS
clean:
	rm TestParser
	rm TestParser.o
	rm Parser.o
