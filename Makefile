.PHONY: clean sssp ubuntu/dir

sssp:
	ghc -outputdir ./tmp --make -O2 -threaded $@.hs -o $@
	strip $@

ubuntu/build: sssp ubuntu/sssp ubuntu/dir

ubuntu/sssp: libs = $(shell ubuntu/util statics)
ubuntu/sssp: ubuntu/util
	ghc -outputdir ./tmp --make -O2 -threaded $<.hs -o $@ \
	 -optl-Wl,--whole-archive \
	  $(libs:%=-optl%) \
	 -optl-Wl,--no-whole-archive
	strip $@

ubuntu/sssp.sig: ubuntu/sssp
	rm -f $@
	gpg --use-agent --detach-sign $<

ubuntu/sssp.sha: ubuntu/sssp
	shasum --portable --algorithm 512 $< > $@

ubuntu/dir: v = $(shell egrep ^version ./sssp.cabal | sed 's/^.*: *//')
ubuntu/dir: arch = $(shell dpkg-architecture -qDEB_BUILD_ARCH)
ubuntu/dir:
	mkdir ubuntu/sssp-$(v)-$(arch)-ubuntu

ubuntu/dist: d = $(wildcard ubuntu/sssp-*-*-ubuntu)
ubuntu/dist: ubuntu/sssp.sig ubuntu/sssp.sha ubuntu/sssp
	tar -cC ubuntu $(^:ubuntu/%=%) | tar -xC $d
	tar -cjf $(d:ubuntu/%=%).tbz -C ubuntu $(d:ubuntu/%=%)

README: doc/index.rst
	( cd doc && make man )
	man doc/.build/man/sssp.1 | col -bx |\
	  sed -n '/SYNOPSIS/,/AUTHOR/ { /AUTHOR/d ; p ;}' > ./README

clean:
	rm -rf tmp ubuntu/tmp sssp ubuntu/sssp
	( cd ./doc/ && make clean )

