mk-cgi:
	(cd cmd; make)

install:
	cabal v1-install --allow-newer

clean:
	rm -f */*.o */*.hi *.o *.hi
	rm -Rf dist
	(cd cmd ; make clean)

clean-all:
	rm -f index.cgi

push-rd:
	darcs push -a rd@rohandrape.net:sw/hcycling

pull-rd:
	darcs pull -a http://rohandrape.net/sw/hcycling

remote-update:
	ssh rd@rohandrape.net "(cd sw/hcycling ; make mk-cgi)"

indent:
	fourmolu -i Cycling cmd

doctest:
	doctest -Wno-x-partial -Wno-incomplete-uni-patterns Cycling

dep:
	cabal v1-install csv --allow-newer
