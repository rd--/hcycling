all:
	ghc -Wall \
		--make cycling-cgi.hs \
		-o ../index.cgi \
		-optl -L$$HOME/lib
	strip -s ../index.cgi

clean:
	rm -f *.hi *.o ../index.cgi
