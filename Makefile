.PHONY: clean sssp links

README: doc/index.rst
	( cd doc && make man )
	man doc/.build/man/sssp.1 | col -bx |\
	  sed -n '/SYNOPSIS/,/AUTHOR/ { /AUTHOR/d ; p ;}' > ./README

sssp:
	ghc -outputdir ./tmp --make -O2 -threaded $@.hs -o $@
	strip $@

ubuntu/sssp: sssp links ubuntu/util
	ghc -outputdir ./ubuntu/tmp --make -O2 -threaded $<.hs -o $@
	strip $@

links: ubuntu/util
	$< links --sudo

clean:
	rm -rf tmp ubuntu/tmp sssp ubuntu/sssp
	( cd ./doc/ && make clean )

