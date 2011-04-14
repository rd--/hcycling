all:
	cabal install
	cp dist/build/cycling-cgi/cycling-cgi index.cgi
	strip -s index.cgi

clean:
	rm -f index.cgi
