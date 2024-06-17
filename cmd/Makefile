all:
	ghc --make cgi.hs -o index.cgi
	strip -s index.cgi
	cp index.cgi ..

install:
	echo "hcycling/cmd"

clean:
	rm -f *.o *.hi index.cgi

clean-all:
	rm -f index.cgi

print-exec:
	hsc3-setup cabal print-exec hycling- *.hs

rm-local:
	rm -f Text WWW

ln-local:
	rm -f Text WWW
	ln -s ~/sw/html-minus/Text Text
	ln -s ~/sw/www-minus/WWW WWW
