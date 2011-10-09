all-cabal:
	cabal install --disable-documentation
	cp dist/build/cycling-cgi/cycling-cgi index.cgi
	strip -s index.cgi

all-ghc:
	ghc --make cycling-cgi.hs -o index.cgi
	strip -s index.cgi

clean:
	rm -f */*.o */*.hi *.o *.hi

clean-all:
	rm -f index.cgi
