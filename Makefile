all: gantt

%: %.hs
	cabal exec ghc -- --make -fwarn-unused-imports $<

%.cgi: %.hs
	cabal exec ghc -- --make  $< -o $@

%.profiled: %.hs
	cabal exec ghc -- --make -prof -auto-all -caf-all -rtsopts  $<

clean:
	rm -f *.hi *.o

gantt: Parse.hs
