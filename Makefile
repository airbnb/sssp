.PHONY: clean sssp sssp.prof

README: doc/index.rst
	( cd doc && make man )
	man doc/.build/man/sssp.1 | col -bx |\
	  sed -n '/SYNOPSIS/,/AUTHOR/ { /AUTHOR/d ; p ;}' > ./README

sssp:
	ghc -outputdir ./tmp --make -O2 $@.hs -o $@
	strip sssp

sssp.prof:
	ghc -outputdir ./tmp --make -rtsopts -prof -auto-all $@.hs -o $@

clean:
	rm -rf tmp sssp sssp.prof
	( cd ./doc/ && make clean )

