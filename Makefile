.PHONY: doc

all:
	mkdir -p ebin
	(cd src;$(MAKE))

doc:
	(cd src; $(MAKE) doc)

test: all
	(cd t;$(MAKE))
	(cd t;$(MAKE) test)

prove: all
	(cd t;$(MAKE))
	prove t/*.t

clean:
	(cd src;$(MAKE) clean)
	(cd t;$(MAKE) clean)

dist-src: clean
	tar zcvf etap-0.3.2.tgz src/ ebin/ support/ Makefile
