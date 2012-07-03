README: doc/index.rst
	( cd doc && make man )
	man doc/.build/man/s3p.1 | col -bx |\
	  sed -n '/SYNOPSIS/,/AUTHOR/ { /AUTHOR/d ; p ;}' > ./README

