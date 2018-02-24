mk-cgi:
	ghc --make cycling-cgi.hs -o index.cgi
	strip -s index.cgi

clean:
	rm -f */*.o */*.hi *.o *.hi
	rm -Rf dist
	(cd cmd ; make clean)

clean-all:
	rm -f index.cgi

push-sp:
	darcs push -a rd@slavepianos.org:sw/hcycling

pull-sp:
	darcs pull -a http://rd.slavepianos.org/sw/hcycling

remote-update:
	ssh rd@slavepianos.org "(cd sw/hcycling ; make mk-cgi)"
