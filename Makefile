
all:
	(cd src;$(MAKE))

test: all
	(cd t;$(MAKE))
	(cd t;$(MAKE) test)

clean:
	(cd src;$(MAKE) clean)
	(cd t;$(MAKE) clean)

dist-src: clean
	tar zcvf ipwcore-0.5.1.tgz src/ ebin/ support/ Makefile ipwcore