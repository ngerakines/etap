
all:
	mkdir -p ebin
	(cd src;$(MAKE))

test: all
	(cd t;$(MAKE))
	(cd t;$(MAKE) test)

clean:
	(cd src;$(MAKE) clean)
	(cd t;$(MAKE) clean)

dist-src: clean
	tar zcvf etap-0.3.1.tgz src/ ebin/ support/ Makefile
