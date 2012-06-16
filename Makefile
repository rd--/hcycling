all-ghc:
	ghc --make cycling-cgi.hs -o index.cgi
	strip -s index.cgi

all-cabal:
	cabal install --disable-documentation
	cp dist/build/cycling-cgi/cycling-cgi index.cgi
	strip -s index.cgi

clean:
	rm -f */*.o */*.hi *.o *.hi
	rm -Rf dist

clean-all:
	rm -f index.cgi

remote-update:
	ssh rd@slavepianos.org "(cd sw/hcycling ; make all-ghc)"
