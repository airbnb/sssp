README: doc/index.rst
	( cd doc && make man )
	man doc/.build/man/sssp.1 | col -bx |\
	  sed -n '/SYNOPSIS/,/AUTHOR/ { /AUTHOR/d ; p ;}' > ./README

sssp: sssp.hs
	ghc -outputdir ./tmp --make -O2 ./sssp.hs -o sssp
	strip sssp

clean:
	rm -rf tmp sssp
	( cd ./doc/ && make clean )

