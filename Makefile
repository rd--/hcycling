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

push-all:
	r.gitlab-push.sh hcycling
	r.github-push.sh hcycling

remote-update:
	ssh rd@rohandrape.net "(cd sw/hcycling ; make mk-cgi)"

indent:
	fourmolu -i Cycling cmd

doctest:
	doctest -Wno-x-partial -Wno-incomplete-uni-patterns Cycling

dep:
	cabal v1-install csv --allow-newer
