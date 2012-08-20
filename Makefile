README: doc/index.rst
	( cd doc && make man )
	man doc/.build/man/sssp.1 | col -bx |\
	  sed -n '/SYNOPSIS/,/AUTHOR/ { /AUTHOR/d ; p ;}' > ./README

sssp: sssp.hs
	ghc -outputdir ./tmp --make -O2 ./$< -o $@
	strip sssp

sssp.prof: sssp.hs
	ghc -outputdir ./tmp --make -rtsopts -prof -auto-all ./$< -o $@

clean:
	rm -rf tmp sssp sssp.prof
	( cd ./doc/ && make clean )

