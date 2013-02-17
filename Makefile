.PHONY: doc clean cabal ubuntu-build-deps tarballs

# Prevents the .sha files from being deleted. Not sure what the story is.
.SECONDARY:

v = $(shell git describe || bin/tags cabal_version || echo '(unversioned)')
tag = sssp-$(shell ./bin/tags tag)
built = $(wildcard tmp/dist/*/sssp)
tarballs = $(built:%/sssp=%.tbz)

ifeq (Darwin,$(shell uname))
  tagged = tmp/sssp.cabal
else
  tagged = tmp/sssp.custom
endif

this_platform: tmp/dist/$(tag)/sssp

version:
	echo $v > $@

tmp/dist/$(tag)/sssp: $(tagged)
	mkdir -p tmp/dist/$(tag)
	mv -f $< $@

tmp/dist/%/sssp.gpg: tmp/dist/%/sssp
	gpg --use-agent --detach-sign $<

tmp/dist/%/sssp.sha: d = $(@:%/sssp.sha=%)
tmp/dist/%/sssp.sha: tmp/dist/%/sssp
	( cd $d && shasum --portable --algorithm 512 sssp > sssp.sha )

tarballs: $(tarballs)

tmp/dist/%.tbz: d = $(@:tmp/dist/%.tbz=%)
tmp/dist/%.tbz: tmp/dist/%/sssp tmp/dist/%/sssp.gpg tmp/dist/%/sssp.sha
	tar cjf $@ -C tmp/dist $d

sssp: version sssp.hs doc
	ghc -outputdir tmp --make -O2 sssp.hs -o sssp

tmp/sssp.custom: libs = $(shell bin/so2a4hs statics sssp)
tmp/sssp.custom: sssp bin/so2a4hs
	ghc -outputdir tmp --make -O2 sssp.hs -o $@ \
	 -optl-Wl,--whole-archive \
	  $(libs:%=-optl%) \
	 -optl-Wl,--no-whole-archive
	strip $@

ubuntu-build-deps:
	env DEBIAN_FRONTEND=noninteractive aptitude install -y \
	  python-sphinx cabal-install
	cabal install --only-dependencies

tmp/sssp.cabal: dist/build/sssp/sssp
	mkdir -p tmp
	cp dist/build/sssp/sssp $@
	strip $@

dist/build/sssp/sssp: cabal

cabal: version
	cabal configure --disable-executable-profiling \
	                --disable-library-profiling
	cabal build

doc: README

README: doc/index.rst
	( cd doc && make man )
	man doc/.build/man/sssp.1 | col -bx |\
	  sed -n '/SYNOPSIS/,/AUTHOR/ { /AUTHOR/d ; p ;}' > ./README

clean:
	rm -rf tmp sssp dist/build
	( cd ./doc/ && make clean )

